;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix workflows execution-order)
  #:use-module (srfi srfi-1)
  #:export (sequential-execution-order
            parallel-step-execution-order
            graph-execution-order
            process-depends-on
            process-needed-by
            compute-free-points
            reduce-dependencies))

(define* (process-depends-on process dependencies #:optional (depend-list '()))
  "Returns the list of processes PROCESS depends on."
  (if (null? dependencies)
    depend-list
    (let ((item (car dependencies)))
      (if (equal? process (cadr item))
        (process-depends-on process (cdr dependencies)
                            (append depend-list (cdr item)))
        (process-depends-on process (cdr dependencies) depend-list)))))

(define* (process-needed-by process dependencies #:optional (depend-list '()))
  "Returns the dependency pairs are needed by PROCESS."
  (if (null? dependencies)
    depend-list
    (let ((item (car dependencies)))
      (if (equal? process (cadr item))
        (process-needed-by process
                           (delete item dependencies)
                           (append depend-list (list item)))
        (process-needed-by process
                           (delete item dependencies)
                           depend-list)))))

(define* (compute-free-points processes dependencies
                              #:optional (free-points '()))
  "Returns a PROCESS that can be used to start the execution at."
  (if (null? processes)
    free-points
    (let ((process (car processes)))
      (if (null? (process-depends-on process dependencies))
        (compute-free-points (cdr processes) dependencies
                             (append free-points (list process)))
        (compute-free-points (cdr processes) dependencies
                             free-points)))))

(define (reduce-dependencies processes dependencies)
  "Removes dependencies on resolved PROCESSES."
  (if (null? processes)
    dependencies
    (reduce-dependencies
     (cdr processes)
     (lset-difference eq? dependencies
                      (process-needed-by (car processes) dependencies)))))

(define* (sequential-execution-order processes dependencies #:optional (order '()))
  "Returns the list of PROCESSES, re-ordered so it can be executed in a sequence
and adhere to the dependencies provided in DEPENDENCIES."
  (if (null? processes)
    (reverse order)
    (let* ((resolvable (compute-free-points processes dependencies))
           (leftovers (lset-difference eq? processes resolvable)))
      (if (null? resolvable)
          #f
          (sequential-execution-order
           leftovers
           (reduce-dependencies resolvable dependencies)
           (append resolvable order))))))

(define (parallel-step-execution-order processes dependencies)
  "Returns the list of PROCESSES grouped by processes that can be executed in
parallel at each step and adhere to the dependencies provided in DEPENDENCIES."

  (define (stepper processes dependencies order)
    (if (null? processes)
        (reverse order)
        (let* ((resolvable (compute-free-points processes dependencies))
               (leftovers (lset-difference eq? processes resolvable)))
          (if (null? resolvable)
              #f
              (stepper
               leftovers
               (reduce-dependencies resolvable dependencies)
               (append (list resolvable) order))))))

  (stepper processes dependencies '()))

(define* (graph-execution-order processes dependencies #:optional (order '()))
  "Returns the list of PROCESSES, ordered such that each list contains a
sequential output of a branch in the dependency graph defined by
DEPENDENCIES."
  
  )

