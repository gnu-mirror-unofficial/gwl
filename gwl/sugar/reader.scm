;;; Copyright © 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl sugar reader)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (reader-extension-inline-code))

(eval-when (expand load compile eval)
  (define (reader-extension-inline-code chr port)
    "When this reader macro is registered for CHR it reads all
characters between code delimiters from PORT and returns a code
snippet.

Here is an example:

    # python {
    print(\"hello\")
    }
    => (code-snippet 'python '()
         (list \"\nprint(\\\"hello\\\")\n\"))

If there is no matching language definition, the first line is
considered as the invocation of an interpreter.

    # /bin/bash -c { echo hello world }
    => (code-snippet '/bin/bash '(\"-c\")
         (list \" echo hello world \"))

This reader macro also supports string interpolation.  Any
uninterrupted string between double curly braces will be turned into a
variable reference.

    # /bin/bash -c {echo {{name}} is great}
    => (code-snippet '/bin/bash '(\"-c\")
         (list \"echo \" name \" is great\"))

When no interpreter is provided it uses /bin/sh:

    # { echo \"hello world\" }
    => (code-snippet 'sh '(\"-c\")
         (list \" echo hello world \"))

"
    ;; Throw away any number of blank characters
    (let loop ((next (lookahead-char port)))
      (and (char-set-contains? char-set:blank next)
           (read-char port)
           (loop (lookahead-char port))))

    ;; This first line must be the language identifier or executable
    ;; path with arguments.
    (match (let ((prelude (string-trim-both (read-delimited "{" port))))
             (or (and=> (string-index prelude #\space)
                        (lambda (index)
                          (cons (substring prelude 0 index)
                                (string-split (substring prelude (1+ index))
                                              #\space))))
                 (list prelude)))
      ((language . arguments)
       (let search-delim ((acc '())
                          (char (read-char port))
                          (balance 1)
                          (chunks '())
                          (maybe-variable? #f))
         ;; TODO: don't use "throw", raise an exception!
         (when (eof-object? char)
           (throw 'inline-code-unbalanced-braces balance))

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
                                                  (if (eq? #\: (car kw))
                                                      ;; multiple items
                                                      `(and=> (memq ,(symbol->keyword
                                                                      (string->symbol
                                                                       (list->string (cdr kw))))
                                                                    ,(string->symbol (list->string pre:)))
                                                              (lambda (sublist)
                                                                (break keyword? (cdr sublist))))
                                                      ;; single item
                                                      `(and=> (memq ,(symbol->keyword
                                                                      (string->symbol (list->string kw)))
                                                                    ,(string->symbol (list->string pre:)))
                                                              cadr))))))))
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
                 `(code-snippet ',(match language
                                    ("" 'sh)
                                    (_ (string->symbol language)))
                                ',arguments
                                (list ,@(reverse (cons last-chunk chunks)))))
               (search-delim new-acc
                             (read-char port)
                             new-balance
                             new-chunks
                             new-maybe-variable?)))))))
  ;; Support syntactic sugar
  (read-hash-extend #\space reader-extension-inline-code)
  (read-hash-extend #\newline reader-extension-inline-code))
