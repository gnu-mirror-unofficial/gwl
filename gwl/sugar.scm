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
  #:export (process:
            workflow:))

;; Shorter syntax, which is especially useful when wisp is used.
(define-syntax process:
  (lambda (x)
    (syntax-case x ()
      ((_ (id . args) rest ...)
       #`(define-public id
           (lambda* args
             (process
              (name #,(symbol->string (syntax->datum #'id)))
              rest ...))))
      ((_ id rest ...)
       #`(define-public id
           (process
            (name #,(symbol->string (syntax->datum #'id)))
            rest ...))))))

;; Shorter syntax, which is especially useful when wisp is used.
(define-syntax workflow:
  (lambda (x)
    (syntax-case x ()
      ((_ id rest ...)
       #`(define-public id
           (workflow
            (name #,(symbol->string (syntax->datum #'id)))
            rest ...))))))

(eval-when (expand load compile eval)
  (define (reader-extension-inline-code chr port)
    "When this reader macro is registered for CHR it reads all
characters between code delimiters from PORT and returns a code
snippet.

Here is an example:

    # python
    print(\"hello\")
    ##
    => (code-snippet 'python \"\" \"print(\\\"hello\\\"))

If there is no matching language definition, the first line is
considered as the invocation of an interpreter.

    # /bin/bash -c
    echo hello world
    ##
    => (code-snippet '/bin/bash \"-c\" \"print(\\\"hello\\\"))
"
    (define delim-end "##\n")
    (define delim-end-first (substring/shared delim-end 0 1))
    (define delim-end-rest (substring/shared delim-end 1))
    (define (read-chunk)
      (get-string-n port (- (string-length delim-end) 1)))

    ;; Throw away any number of blank characters
    (let loop ((next (lookahead-char port)))
      (if (char-set-contains? char-set:blank next)
          (begin (read-char port)
                 (loop (lookahead-char port)))
          #t))

    ;; This first line must be the language identifier or executable
    ;; path with arguments.
    (match (let ((line (get-line port)))
             (or (and=> (string-index line #\space)
                        (lambda (index)
                          (cons (substring line 0 index)
                                (substring line (1+ index)))))
                 (cons line "")))
      (("" . _)
       (throw 'inline-code-language-undefined))
      ((language . arguments)
       (let search-delim ((acc (list (read-delimited delim-end-first port)))
                          (chunk (read-chunk)))
         (if (string= chunk delim-end-rest)
             `(code-snippet ',(string->symbol language)
                            ',(string-split arguments #\space)
                            ,(string-join (reverse! acc) delim-end-first))
             (begin
               (unget-string port chunk)
               (search-delim (cons (read-delimited delim-end-first port) acc)
                             (read-chunk))))))))
  ;; Support syntactic sugar
  (read-hash-extend #\space reader-extension-inline-code)
  (read-hash-extend #\newline reader-extension-inline-code))
