;;; Copyright © 2016  Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (gwl www util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (syntax-highlight scheme)
  #:use-module (syntax-highlight lexers)
  #:export (directory?
            file-extension
            string-replace-occurrence
            request-path-components

            lex-gwl))

(define (directory? filename)
  (string=? filename (dirname filename)))

(define (file-extension file-name)
  (last (string-split file-name #\.)))

(define (string-replace-occurrence str occurrence alternative)
  (string-map (lambda (x) (if (eq? x occurrence) alternative x)) str))

(define request-path-components
  (compose split-and-decode-uri-path uri-path request-uri))


(define char-set:lisp-delimiters
  (char-set-union char-set:whitespace
                  (char-set #\# #\newline #\( #\) #\[ #\] #\{ #\})))

(define char-set:lisp-symbol
  (char-set-complement char-set:lisp-delimiters))

(define (lex-special-symbol sym)
  (lex-filter (lambda (str)
                (string=? sym str))
              (lex-char-set char-set:lisp-symbol)))

(define (lex-special-symbol symbols prefixes)
  (lex-filter (lambda (str)
                (or (any (cut string=? symbols <>) symbols)
                    (any (cut string-prefix? <> str) prefixes)))
              (lex-char-set char-set:lisp-symbol)))

(define (lex-map2 proc lexer)
  (lambda (tokens cursor)
    (let-values (((result remainder) (lexer tokens cursor)))
      (if result
          (match (token-take result 2)
            ((second first) ; accumulator tokens are in reverse order
             (values (token-add (token-drop result 2)
                                (proc first second))
                     remainder)))
          (fail)))))

(define gwl-special-symbols
  (cons* "process"
         "workflow"

         "with"

         %default-special-symbols))

(define scheme-functions
  (list "append-map"
        "append"
        "basename"
        "zip"
        "map"
        "string-append"))

(define gwl-fields
  (list "name"
        "packages"
        "inputs"
        "outputs"
        "run-time"
        "processes"))

(define (make-gwl-lexer special-symbols special-prefixes)
  "Return a lexer that highlights GWL source code.  Tag strings
that are in SPECIAL-SYMBOLS or match one of the string prefixes in
SPECIAL-PREFIXES with the 'special' tag."
  (lex-consume
   (lex-any (lex-tag 'line (lex-regexp "$\n"))
            (lex-char-set char-set:whitespace)
            (lex-tag 'code-snippet-start
                     (lex-regexp "# [^ ]* ?\\{"))
            (lex-tag 'code-snippet-end
                     (lex-string "}"))
            (lex-tag 'placeholder
                     (lex-delimited "{{" #:until "}}"))
            (lex-tag 'open (lex-any* (map lex-string '("(" ))))
            (lex-tag 'close (lex-any* (map lex-string '(")"))))
            (lex-tag 'comment (lex-delimited ";" #:until "\n"))
            (lex-tag 'multi-line-comment
                     (lex-delimited "#|" #:until "|#" #:nested? #t))
            (lex-tag 'gwl-field
                     (lex-filter (lambda (str)
                                   (any (cut string=? <> str)
                                        gwl-fields))
                                 (lex-char-set char-set:lisp-symbol)))
            (lex-tag 'function
                     (lex-filter (lambda (str)
                                   (any (cut string=? <> str)
                                        scheme-functions))
                                 (lex-char-set char-set:lisp-symbol)))
            (lex-tag 'special
                     (lex-filter (lambda (str)
                                   (or (any (cut string=? <> str)
                                            special-symbols)
                                       (any (cut string-prefix? <> str)
                                            special-prefixes)))
                                 (lex-char-set char-set:lisp-symbol)))
            (lex-tag 'string (lex-delimited "\""))
            (lex-tag 'keyword
                     (lex-any (lex-map2 string-append
                                        (lex-all (lex-string "#:")
                                                 (lex-char-set char-set:lisp-symbol)))
                              (lex-map2 string-append
                                        (lex-all (lex-char-set char-set:lisp-symbol)
                                                 (lex-string ":")))))
            (lex-tag 'symbol
                     (lex-any (lex-delimited "#{" #:until "}#")
                              (lex-char-set char-set:lisp-symbol))) )))

(define lex-gwl
  (make-gwl-lexer gwl-special-symbols
                  (@@ (syntax-highlight scheme) %default-special-prefixes)))
