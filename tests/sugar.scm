;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (test-sugar)
  #:use-module (gwl sugar)
  #:use-module (gwl processes)
  #:use-module (srfi srfi-64))

(test-begin "sugar")

(define (convert str)
  (call-with-input-string str
    (lambda (port)
      ((@@ (gwl sugar) reader-extension-inline-code)
       #\# port))))

(test-equal "reader supports string interpolation"
  '(code-snippet (quote foo)
                 (quote ("bar" "baz"))
                 (apply string-append (list " print(\"hello " world "\") ")))
  (convert "foo bar baz { print(\"hello {{world}}\") }"))

(test-equal "reader will not interpolate values with spaces"
  '(code-snippet (quote foo)
                 (quote ("bar" "baz"))
                 (apply string-append (list " print(\"hello {{not a variable" "}" "}\") ")))
  (convert "foo bar baz { print(\"hello {{not a variable}}\") }"))

(test-error "reader complains about unbalanced curlies"
            '(inline-code-unbalanced-braces 3)
            (convert "foo { this { is { garbage"))

(test-error "reader complains about unbalanced curlies"
            '(inline-code-unbalanced-braces 1)
            (convert "foo { this { is { still garbage }}"))

(test-error "reader complains about unspecified language"
            'inline-code-language-undefined
            (convert " { what is this? }"))

(define name "Bender")
(test-equal "string interpolation works"
  "echo Bender is great"
  (code-snippet-code
   (test-read-eval-string "# /bin/bash -c {echo {{name}} is great}")))
(test-equal "string interpolation does not kill bash variable access"
  "echo ${name} is great"
  (code-snippet-code
   (test-read-eval-string "# /bin/bash -c {echo ${name} is great}")))

(test-end "sugar")
