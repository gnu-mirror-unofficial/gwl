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

(define-module (test-processes)
  #:use-module (gwl processes)
  #:use-module (gwl sugar reader)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1) ; this is available at run time
  #:use-module (srfi srfi-64))

(test-begin "processes")

(test-assert "make-process macro supports implicit lists"
  (let ((proc (make-process
               (name "anything")
               (procedure '(const #t))
               (inputs 'this 'that 'whatever))))
    (equal? '(this that whatever)
            (process-inputs proc))))

(test-assert "make-process macro flattens implicit lists"
  (let ((proc (make-process
               (name "anything")
               (procedure '(const #t))
               (inputs '(this and) 'that '(whatever you say)))))
    (equal? '(this and that whatever you say)
            (process-inputs proc))))

(test-assert "make-process macro supports implicit concatenation"
  (let ((proc (make-process
               (name "anything" " is " "possible")
               (procedure '(const #t)))))
    (equal? "anything is possible"
            (process-name proc))))

(test-assert "make-process implicit concatenation does not break variable references 1/2"
  (let* ((whatever "my-name")
         (proc (make-process
                (name whatever)
                (procedure '(const #t)))))
    (equal? "my-name"
            (process-name proc))))

(test-assert "make-process implicit concatenation does not break variable references 2/2"
  (let* ((whatever "my-name")
         (proc (make-process
                (name "this is " whatever)
                (procedure '(const #t)))))
    (equal? "this is my-name"
            (process-name proc))))

(test-assert "make-process macro supports implicit lists that are already lists"
  (let ((proc (make-process
               (name "anything")
               (procedure '(const #t))
               (inputs (list 'this 'that 'whatever)))))
    (equal? '(this that whatever)
            (process-inputs proc))))

(test-error "make-process validates complexity type 1/2" #t
            (make-process
             (name "anything")
             (procedure '(const #t))
             (run-time 'invalid)))

(test-assert "make-process validates complexity type 2/2"
  (process? (make-process
             (name "anything")
             (procedure '(const #t))
             (run-time (complexity (threads 2))))))

(test-equal "make-process rejects invalid field names 1/3"
  '("process: extraneous fields: garbage \n")
  (catch 'misc-error
    (lambda ()
      (test-read-eval-string (format #false "~s"
                                     '(make-process
                                       (name "anything")
                                       (procedure '(const #t))
                                       (garbage 'this-is)))))
    (lambda (key _ format-string message . rest)
      message)))

(test-equal "make-process rejects invalid field names 2/3"
  "process: Invalid field name"
  (catch 'syntax-error
    (lambda ()
      (test-read-eval-string (format #false "~s"
                                     '(make-process
                                       (name "anything")
                                       (procedure '(const #t))
                                       (#:garbage 'this-is)))))
    (lambda (key _ message . rest)
      message)))

(test-equal "make-process rejects invalid field names 3/3"
  "process: Invalid field name"
  (catch 'syntax-error
    (lambda ()
      (test-read-eval-string (format #false "~s"
                                     '(make-process
                                       (name "anything")
                                       (procedure '(const #t))
                                       (#:garbage 'this-is 'still-garbage)))))
    (lambda (key _ message . rest)
      message)))

(test-assert "make-process permits definitions in field values"
  (let* ((proc (make-process
                (name "bash")
                (procedure
                 (define who "world")
                 (define why "I love you")
                 # {echo "hello {{who}}.  {{why}}."})))
         (snippet (process-procedure proc)))
    (code-snippet? snippet)))

(test-assert "code-snippet is applicable and returns itself"
  (let ((snippet (code-snippet 'sh '() (list))))
    (and (code-snippet? snippet)
         (eq? snippet (snippet))
         (eq? snippet ((snippet)))
         (eq? snippet (((snippet)))))))

(test-error "complexity rejects invalid field names" #t
            (complexity
             (time 10 seconds)
             (hot-dogs 20)))

(test-assert "compile-procedure supports Python code"
  (let* ((proc (make-process
                (name "python")
                (procedure
# python {
print "hello from python 2"
})))
         (snippet (process-procedure proc)))
    (and (code-snippet? snippet)
         (eq? 'python (code-snippet-language snippet))
         (pair? (compile-procedure proc)))))

(test-assert "compile-procedure supports R code"
  (let* ((proc (make-process
                (name "r")
                (procedure
# R {
cat("hello from R")
}
)))
         (snippet (process-procedure proc)))
    (and (code-snippet? snippet)
         (eq? 'R (code-snippet-language snippet))
         (pair? (compile-procedure proc)))))

(test-assert "compile-procedure supports any kind of code"
  (let* ((proc (make-process
                (name "bash")
                (procedure # /bin/bash -c { echo "hello from bash" })))
         (snippet (process-procedure proc)))
    (and (code-snippet? snippet)
         (eq? '/bin/bash (code-snippet-language snippet))
         (equal? '("-c") (code-snippet-arguments snippet))
         (pair? (compile-procedure proc)))))

(test-end "processes")
