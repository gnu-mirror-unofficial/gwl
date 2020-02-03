;;; Copyright © 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gwl sugar reader)
  #:use-module (gwl processes)
  #:use-module (srfi srfi-64))

(test-begin "sugar")

(define (convert str)
  (call-with-input-string str
    (lambda (port)
      (reader-extension-inline-code #\# port))))

(test-equal "reader supports string interpolation"
  '(code-snippet (quote foo)
                 (quote ("bar" "baz"))
                 (begin
                   (use-modules (ice-9 format))
                   (apply string-append
                          (map (lambda (val)
                                 (cond
                                  ((string? val) val)
                                  ((list? val) (format #f "~{~a~^ ~}" val))
                                  (else (format #f "~a" val))))
                               (list " print(\"hello " world "\") ")))))
  (convert "foo bar baz { print(\"hello {{world}}\") }"))

(test-equal "reader supports string interpolation for named references"
  '(code-snippet (quote foo)
                 (quote ("bar" "baz"))
                 (begin
                   (use-modules (ice-9 format))
                   (apply string-append
                          (map (lambda (val)
                                 (cond
                                  ((string? val) val)
                                  ((list? val) (format #f "~{~a~^ ~}" val))
                                  (else (format #f "~a" val))))
                               (list " print(\"hello "
                                     (and=> (memq #:europe world) cadr)
                                     "\") ")))))
  (convert "foo bar baz { print(\"hello {{world:europe}}\") }"))

(test-equal "reader will not interpolate values with spaces"
  '(code-snippet (quote foo)
                 (quote ("bar" "baz"))
                 (begin
                   (use-modules (ice-9 format))
                   (apply string-append
                          (map (lambda (val)
                                 (cond
                                  ((string? val) val)
                                  ((list? val) (format #f "~{~a~^ ~}" val))
                                  (else (format #f "~a" val))))
                               (list " print(\"hello {{not a variable" "}" "}\") ")))))
  (convert "foo bar baz { print(\"hello {{not a variable}}\") }"))

(test-error "reader complains about unbalanced curlies"
            '(inline-code-unbalanced-braces 3)
            (convert "foo { this { is { garbage"))

(test-error "reader complains about unbalanced curlies"
            '(inline-code-unbalanced-braces 1)
            (convert "foo { this { is { still garbage }}"))

(test-equal "reader defaults to /bin/sh"
  '(code-snippet (quote sh)
                 (quote (""))
                 (begin
                   (use-modules (ice-9 format))
                   (apply string-append
                          (map (lambda (val)
                                 (cond
                                  ((string? val) val)
                                  ((list? val) (format #f "~{~a~^ ~}" val))
                                  (else (format #f "~a" val))))
                               (list " just the default ")))))
  (convert " { just the default }"))

(define name "Bender")
(test-equal "string interpolation works"
  "echo Bender is great"
  (code-snippet-code
   (test-read-eval-string "# /bin/bash -c {echo {{name}} is great}")))
(test-equal "string interpolation does not kill bash variable access"
  "echo ${name} is great"
  (code-snippet-code
   (test-read-eval-string "# /bin/bash -c {echo ${name} is great}")))

(define numbers
  (list 100 200 #:my-number 300 400 500 #:boring 1000))
(test-equal "string interpolation can access named items in lists"
  "echo my number is 300, not 1000"
  (code-snippet-code
   (test-read-eval-string "# /bin/bash -c {echo my number is {{numbers:my-number}}, not {{numbers:boring}}}")))

(test-assert "process macro allows key-less procedure"
  (let ((proc (process
               (name "anything")
               (inputs 'this 'that 'whatever)
               # bash { echo "hello" })))
    (code-snippet? (process-procedure proc))))

(test-assert "process macro allows nested key-less procedure"
  (let ((proc (process
               (name "anything")
               (inputs 'this 'that 'whatever)
               (# bash { echo "hello" }))))
    (code-snippet? (process-procedure proc))))

(test-assert "code snippet has access to process fields"
  (let ((proc (process
               (name "anything")
               (inputs 'this 'that 'whatever)
               # bash { echo {{inputs}} })))
    (code-snippet-code (process-procedure proc))))

(test-end "sugar")
