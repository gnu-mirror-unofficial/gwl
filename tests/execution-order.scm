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

(define-module (test-execution-order)
  #:use-module (gwl processes)
  #:use-module (gwl workflows)
  #:use-module (gwl workflows execution-order)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

(test-begin "execution-order")

;; 1 -> 2 -\
;;   -> 3 -> 5 --> 6
;; 4 ------/

(define p1 (process (name "p1") (procedure '())))
(define p2 (process (name "p2") (procedure '())))
(define p3 (process (name "p3") (procedure '())))
(define p4 (process (name "p4") (procedure '())))
(define p5 (process (name "p5") (procedure '())))
(define p6 (process (name "p6") (procedure '())))

(define wf
  (workflow
   (name "test-workflow")
   (processes
    (list p1 p2 p3 p4 p5 p6))
   (restrictions
    (list (list p2 p1)
          (list p3 p1)
          (list p5 p2 p3 p4)
          (list p6 p5)))))

(define wf2
  (workflow
   (name "test-workflow")
   (processes
    (graph (p2 -> p1)
           (p3 -> p1)
           (p5 -> p2 p3 p4)
           (p6 -> p5)))))

(test-equal "deprecated restrictions can be expressed with processes field"
  (workflow-restrictions wf2)
  (workflow-restrictions wf))

(test-assert "sequential-execution-order"
  (let* ((order (sequential-execution-order
                 (workflow-processes wf)
                 (workflow-restrictions wf)))
         (first-then (lambda (a b)
                       (call-with-values
                           (lambda ()
                             (partition (cut eq? a <>) order))
                         (lambda (before after)
                           (find (cut eq? b <>) after))))))
    (and (first-then p1 p2)
         (first-then p1 p3)
         (first-then p1 p5)
         (first-then p1 p6)
         (first-then p4 p5)
         (first-then p4 p6)
         (first-then p2 p5)
         (first-then p2 p6)
         (first-then p3 p5)
         (first-then p3 p6))))

(test-assert "parallel-step-execution-order"
  (every (lambda (a b) (lset= eq? a b))
         (parallel-step-execution-order
          (workflow-processes wf)
          (workflow-restrictions wf))
         (list (list p1 p4)
               (list p2 p3)
               (list p5)
               (list p6))))

;; The (wisp) module thinks its called as a script when (command-line)
;; is a list of more than one value...
(set-program-arguments '())
(use-modules (wisp))

(test-equal "wisp syntax produces the expected S-expression"
  (with-input-from-string (wisp2lisp "
workflow
  name \"test-workflow\"
  processes
    graph
      p2 -> p1
      p3 -> p1
      p5 -> p2 p3 p4
      p6 -> p5
")
    (lambda ()
      (read (current-input-port))))
  '(workflow
    (name "test-workflow")
    (processes
     (graph (p2 -> p1)
            (p3 -> p1)
            (p5 -> p2 p3 p4)
            (p6 -> p5)))))

(test-end "execution-order")
