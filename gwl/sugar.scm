;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gwl sugar)
  #:use-module (gwl processes)
  #:use-module (gwl workflows)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (process:
            workflow:))

;; Shorter syntax, which is especially useful when wisp is used.
(define-syntax process:
  (lambda (x)
    (syntax-case x ()
      ((_ (id . args) rest ...)
       (if (assoc-ref (syntax->datum #'(rest ...)) 'name)
           #`(define-public id
               (lambda* args
                 (process rest ...)))
           (with-syntax ((the-name (datum->syntax x 'name)))
             #`(define-public id
                 (let ((the-name #,(symbol->string (syntax->datum #'id))))
                   (lambda* args
                     (process
                      (name the-name)
                      rest ...)))))))
      ((_ id rest ...)
       (with-syntax ((the-name (datum->syntax x 'name)))
         #`(define-public id
             (let ((the-name #,(symbol->string (syntax->datum #'id))))
               (process
                (name the-name)
                rest ...))))))))

;; Shorter syntax, which is especially useful when wisp is used.
(define-syntax workflow:
  (lambda (x)
    (syntax-case x ()
      ((_ id rest ...)
       #`(begin
           (define-public id
             (workflow
              (name #,(symbol->string (syntax->datum #'id)))
              rest ...))
           id)))))

(eval-when (expand load compile eval)
  (define (reader-extension-inline-code chr port)
    "When this reader macro is registered for CHR it reads all
characters between code delimiters from PORT and returns a code
snippet.

Here is an example:

    # python {
    print(\"hello\")
    }
    => (code-snippet 'python '(\"\")
         (apply string-append (list \"print(\\\"hello\\\"))))

If there is no matching language definition, the first line is
considered as the invocation of an interpreter.

    # /bin/bash -c { echo hello world }
    => (code-snippet '/bin/bash '(\"-c\")
         (apply string-append (list \" echo hello world \"))))

This reader macro also supports string interpolation.  Any
uninterrupted string between double curly braces will be turned into a
variable reference.

    # /bin/bash -c {echo {{name}} is great}
    => (code-snippet '/bin/bash \"-c\"
         (apply string-append (list \"echo \" name \" is great\")))

"
    ;; Throw away any number of blank characters
    (let loop ((next (lookahead-char port)))
      (if (char-set-contains? char-set:blank next)
          (begin (read-char port)
                 (loop (lookahead-char port)))
          #t))

    ;; This first line must be the language identifier or executable
    ;; path with arguments.
    (match (let ((prelude (string-trim-both (read-delimited "{" port))))
             (or (and=> (string-index prelude #\space)
                        (lambda (index)
                          (cons (substring prelude 0 index)
                                (substring prelude (1+ index)))))
                 (cons prelude "")))
      (("" . _)
       (throw 'inline-code-language-undefined))
      ((language . arguments)
       (let search-delim ((acc '())
                          (char (read-char port))
                          (balance 1)
                          (chunks '())
                          (maybe-variable? #f))
         (match char
           ((? eof-object? c)
            (throw 'inline-code-unbalanced-braces balance))
           (_
            (let-values
                (((new-balance new-chunks new-acc new-maybe-variable?)
                  (match char
                    (#\{
                     (values (1+ balance)
                             chunks
                             (cons char acc)
                             ;; If previous char was also #\{ we are
                             ;; probably reading a variable reference
                             ;; next.
                             (match acc
                               ((#\{ . _) #t)
                               (_ #f))))
                    (#\}
                     (call-with-values
                         (lambda () (break (cut eq? #\{ <>) acc))
                       (lambda (pre post)
                         (if (and maybe-variable?
                                  (not (null? post))
                                  (>= (length post) 2)
                                  (equal? (take post 2) '(#\{ #\{))
                                  (eq? (peek-char port) #\})
                                  (not (any (cut char-set-contains? char-set:whitespace <>) pre)))
                             ;; This is a variable reference
                             (begin
                               (read-char port) ; drop the closing "}"
                               (values (- balance 2)
                                       (let ((reference
                                              ;; Variables may access named
                                              ;; values with ":name".
                                              (call-with-values (lambda () (break (cut eq? #\: <>)
                                                                             (reverse pre)))
                                                (lambda (pre: post:)
                                                  (match post:
                                                    ;; Simple variable identifier
                                                    (() (string->symbol (list->string pre:)))
                                                    ;; Complex identifier.
                                                    ((_ . kw)
                                                     `(and=> (memq ,(symbol->keyword
                                                                     (string->symbol (list->string kw)))
                                                                   ,(string->symbol (list->string pre:)))
                                                             cadr)))))))
                                         (cons* reference
                                                ;; Drop the opening "{{"
                                                (list->string (reverse (drop post 2)))
                                                chunks))
                                       '()
                                       #f))
                             ;; Not a variable
                             (values (1- balance)
                                     (cons (list->string (reverse acc))
                                           chunks)
                                     (list char)
                                     #f)))))
                    (_ (values balance chunks
                               (cons char acc)
                               maybe-variable?)))))
              (if (zero? new-balance)
                  (let ((last-chunk (list->string (reverse acc))))
                    `(code-snippet ',(string->symbol language)
                                   ',(string-split arguments #\space)
                                   (begin
                                     (use-modules (ice-9 format))
                                     (apply string-append
                                            (map (lambda (val)
                                                   (cond
                                                    ((string? val) val)
                                                    ((list? val)
                                                     (format #f "~{~a~^ ~}" val))
                                                    (else (format #f "~a" val))))
                                                 (list ,@(reverse (cons last-chunk chunks))))))))
                  (search-delim new-acc
                                (read-char port)
                                new-balance
                                new-chunks
                                new-maybe-variable?)))))))))
  ;; Support syntactic sugar
  (read-hash-extend #\space reader-extension-inline-code)
  (read-hash-extend #\newline reader-extension-inline-code))
