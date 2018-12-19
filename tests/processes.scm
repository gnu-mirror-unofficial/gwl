;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix processes)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-64))

(test-begin "processes")

(test-assert "procedure->gexp supports s-expressions"
  (let ((proc (process
               (name "s-exp")
               (procedure '(display "hello")))))
    (equal? (procedure->gexp proc)
            (process-procedure proc))))

(test-assert "procedure->gexp supports Python 2 code"
  (let* ((proc (process
                (name "python2")
                (procedure
#---{python2}
print "hello from python 2"
---
)))
         (snippet (process-procedure proc)))
    (and (code-snippet? snippet)
         (eq? 'python2 (code-snippet-language snippet))
         (gexp? (procedure->gexp proc)))))

(test-assert "procedure->gexp supports Python 3 code"
  (let* ((proc (process
                (name "python3")
                (procedure
#---{python}
print("hello from python 3")
---
)))
         (snippet (process-procedure proc)))
    (and (code-snippet? snippet)
         (eq? 'python (code-snippet-language snippet))
         (gexp? (procedure->gexp proc)))))

(test-assert "procedure->gexp supports R code"
  (let* ((proc (process
                (name "r")
                (procedure
#---{r}
cat("hello from R")
---
)))
         (snippet (process-procedure proc)))
    (and (code-snippet? snippet)
         (eq? 'r (code-snippet-language snippet))
         (gexp? (procedure->gexp proc)))))

(test-end "processes")
