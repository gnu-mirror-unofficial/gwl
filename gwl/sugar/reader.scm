;;; Copyright © 2018, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (gwl errors)
  #:export (reader-extension-inline-code))

(eval-when (expand load compile eval)
  (define (process-placeholder
           char balance chunks acc maybe-variable? port)
    "Determine if a chunk enclosed in {{...}} is a reference to a
variable and process it."
    (let-values (((pre post) (break (cut eq? #\{ <>) acc)))
      (if (and maybe-variable?
               (not (null? post))
               (>= (length post) 2)
               (equal? (take post 2) '(#\{ #\{))
               (eq? (peek-char port) #\})
               (not (any (cut char-set-contains? char-set:whitespace <>) pre)))
          (begin
            (read-char port)            ; drop the closing "}"
            (let ((new-balance (cdr (cdr balance)))
                  ;; Variables may access named
                  ;; values with ":name".
                  (reference
                   (let*-values (((pre: post:)
                                  (break (cut eq? #\: <>)
                                         (reverse pre)))
                                 ((variable maybe-unquoted)
                                  (match (string->symbol (list->string pre:))
                                    ;; Look up process fields in the
                                    ;; script arguments at runtime
                                    ((and (or 'inputs 'outputs 'name) field)
                                     (values `(assoc-ref #{ %gwl process-arguments}# ',field)
                                             identity))
                                    ;; Other values should be unquoted right away
                                    (it
                                     (values it
                                             (lambda (val)
                                               (list 'unquote val)))))))
                     (maybe-unquoted
                      (match post:
                        ;; Simple variable identifier
                        (() variable)
                        ;; Complex identifier, multiple items
                        ((_ #\: . kw)
                         (let ((key (list->string kw)))
                           `(or (and=> (memq ,(symbol->keyword
                                               (string->symbol key))
                                             ,variable)
                                       (lambda (sublist)
                                         (break keyword? (cdr sublist))))
                                (error (format #false "Could not access `~a' in `~a'~%"
                                               ,key ,variable)))))
                        ;; Complex identifier, single item
                        ((_ . kw)
                         (let ((key (list->string kw)))
                           `(or (and=> (memq ,(symbol->keyword
                                               (string->symbol key))
                                             ,variable)
                                       cadr)
                                (error (format #false "Could not access `~a' in `~a'~%"
                                               ,key ,variable))))))))))
              (values
               ;; new-balance
               (cdr (cdr balance))
               ;; new-chunks
               (cons* reference
                      ;; Drop the opening "{{"
                      (list->string (reverse (drop post 2)))
                      chunks)
               ;; new-acc
               '()
               ;; new-maybe-variable?
               #f)))

          ;; Not a variable
          ;; TODO: should this be an error?
          (values (cdr balance)
                  (cons (list->string (reverse acc))
                        chunks)
                  (list char)
                  #f))))

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
                          (balance (list (seek port 0 SEEK_CUR)))
                          (chunks '())
                          (maybe-variable? #f))
         (when (eof-object? char)
           (let* ((current-position (car balance))
                  (where (begin
                           (seek port 0 SEEK_SET)
                           (let count-newlines ((lines 1)
                                                (columns 0)
                                                (remaining current-position))
                             (if (zero? remaining)
                                 (location (port-filename port) lines columns)
                                 (let-values
                                     (((new-lines new-columns)
                                       (match (read-char port)
                                         (#\newline (values (1+ lines) 0))
                                         (_ (values lines (1+ columns))))))
                                   (count-newlines new-lines new-columns
                                                   (1- remaining))))))))
             (raise (condition
                     (&gwl-error)
                     (&gwl-syntax-error
                      (location where))
                     (&formatted-message
                      (format "Missing ~a closing brace(s).~%")
                      (arguments (list (length balance))))))))

         (let-values
             (((new-balance new-chunks new-acc new-maybe-variable?)
               (match char
                 (#\{
                  (values (cons (seek port 0 SEEK_CUR) balance)
                          chunks
                          (cons char acc)
                          ;; If previous char was also #\{ we are
                          ;; probably reading a variable reference
                          ;; next.
                          (match acc
                            ((#\{ . _) #t)
                            (_ #f))))
                 (#\}
                  (process-placeholder
                   char balance chunks acc maybe-variable? port))
                 (_ (values balance chunks
                            (cons char acc)
                            maybe-variable?)))))
           (if (null? new-balance)
               (let ((last-chunk (list->string (reverse acc))))
                 `(code-snippet ',(match language
                                    ("" 'sh)
                                    (_ (string->symbol language)))
                                ',arguments
                                ;; XXX: hack to generate a quasiquoted
                                ;; expression without quasiquoting the
                                ;; code here.
                                (,(symbol-append 'quasi 'quote)
                                 ,(reverse (cons last-chunk chunks)))))
               (search-delim new-acc
                             (read-char port)
                             new-balance
                             new-chunks
                             new-maybe-variable?)))))))
  ;; Support syntactic sugar
  (read-hash-extend #\space reader-extension-inline-code)
  (read-hash-extend #\newline reader-extension-inline-code))
