;;; Copyright © 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gwl errors)
  #:use-module (gwl processes)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-64))

(test-begin "sugar")

(define (convert str)
  (call-with-input-string str
    (lambda (port)
      (reader-extension-inline-code #\# port))))

;; This is what snippet-caller does internally.
(define (code-as-string code)
  (string-join (map (lambda (val)
                      (if (list? val)
                          (format #f "~{~a~^ ~}" val)
                          (format #f "~a" val)))
                    code)
               ""))

(test-equal "reader supports embedding variables"
  '(code-snippet (quote foo)
                 (quote ("bar" "baz"))
                 `(" print(\"hello " ,world "\") "))
  (convert "foo bar baz { print(\"hello {{world}}\") }"))

(test-assert "reader ignores leading spaces"
  (let ((snippet (test-read-eval-string "# \n    \n /bin/bash -c {}")))
    (and (equal? (code-snippet-arguments snippet) '("-c"))
         (equal? (code-snippet-language snippet) '/bin/bash))))

(define some-list '("Bender" "Leela" "Fry"))
(test-equal "reader supports embedding of lists"
  "echo Bender Leela Fry are great"
  (code-as-string
   (code-snippet-code
    (test-read-eval-string "# /bin/bash -c {echo {{some-list}} are great}"))))

(test-equal "reader will not interpolate values with spaces"
  "print(\"hello {{not a variable}}\")"
  (code-as-string
   (code-snippet-code
    (test-read-eval-string "# foo bar baz {print(\"hello {{not a variable}}\")}"))))

(test-equal "reader complains about unbalanced curlies"
  '(3)
  (guard (c ((gwl-syntax-error? c)
             (formatted-message-arguments c)))
    (convert "foo { this { is { garbage")))

(test-equal "reader complains about unbalanced curlies"
  '(1)
  (guard (c ((gwl-syntax-error? c)
             (formatted-message-arguments c)))
    (convert "foo { this { is { still garbage }}")))

(test-assert "reader defaults to /bin/sh"
  (let ((snippet (test-read-eval-string "# { just the default }")))
    (and (eq? 'sh (code-snippet-language snippet))
         (null? (code-snippet-arguments snippet)))))

(define who "Bender")
(test-equal "string interpolation works"
  "echo Bender is great"
  (code-as-string
   (code-snippet-code
    (test-read-eval-string "# /bin/bash -c {echo {{who}} is great}"))))
(test-equal "string interpolation does not kill bash variable access"
  "echo ${who} is great"
  (code-as-string
   (code-snippet-code
    (test-read-eval-string "# /bin/bash -c {echo ${who} is great}"))))

(define numbers
  (list 100 200 #:my-number 300 400 500 #:boring 1000))
(test-equal "placeholders can refer to named items in lists"
  '("echo my number is " 300 ", not " 1000 "")
  (code-snippet-code
   (test-read-eval-string "# /bin/bash -c {echo my number is {{numbers:my-number}}, not {{numbers:boring}}}")))

(test-equal "placeholders can refer to tagged consecutive items in lists"
  '("echo my numbers are " (300 400 500) ", not " 1000 "")
  (code-snippet-code
   (test-read-eval-string "# /bin/bash -c {echo my numbers are {{numbers::my-number}}, not {{numbers:boring}}}")))

(test-equal "references to process arguments are delayed"
  '("echo " (assoc-ref #{ %gwl process-arguments}# 'inputs) "")
  (code-snippet-code
   (test-read-eval-string "# {echo {{inputs}}}")))

(let ((#{ %gwl process-arguments}#
       '((inputs . (#:first "1st" #:second "2nd" #:rest "who" "cares"))
         (outputs . ("just this one"))
         (name . "test-process"))))
  (test-equal "references to elements of process arguments are delayed"
    '("echo "
      (or (and=> (memq #:first (assoc-ref #{ %gwl process-arguments}# (quote inputs))) cadr)
          (error (format #f "Could not access `~a' in `~a'~%"
                         "first"
                         (assoc-ref #{ %gwl process-arguments}# (quote inputs)))))
      " "
      (assoc-ref #{ %gwl process-arguments}# (quote inputs))
      " "
      (assoc-ref #{ %gwl process-arguments}# (quote outputs))
      " "
      (assoc-ref #{ %gwl process-arguments}# (quote name)) "")
    (code-snippet-code
     (test-read-eval-string "# {echo {{inputs:first}} {{inputs}} {{outputs}} {{name}}}")))

  (test-equal "references to elements of other variables are not delayed"
    '("echo " 1000 " " "Bender" "")
    (code-snippet-code
     (test-read-eval-string "# {echo {{numbers:boring}} {{who}}}")))

  (test-error "references to unknown elements of variables triggers an error" #true
              (code-snippet-code
               (test-read-eval-string "# {echo {{numbers:last}}}"))))

(test-assert "make-process macro allows key-less procedure"
  (let ((proc (make-process
               (name "anything")
               (inputs 'this 'that 'whatever)
               # bash { echo "hello" })))
    (code-snippet? (process-procedure proc))))

(test-assert "make-process macro allows nested key-less procedure"
  (let ((proc (make-process
               (name "anything")
               (inputs 'this 'that 'whatever)
               (# bash { echo "hello" }))))
    (code-snippet? (process-procedure proc))))

(test-assert "code snippet has access to process fields"
  (let ((proc (make-process
               (name "anything")
               (inputs 'this 'that 'whatever)
               # bash { echo {{inputs}} })))
    (code-snippet-code (process-procedure proc))))

(test-end "sugar")
