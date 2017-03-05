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
            compute-free-points
            resolve-dependency
            resolve-dependencies))

(define (resolve-dependency process dependencies)
  "Remove PROCESS from the list of DEPENDENCIES."
  ;; Remove the resolved dependency.
  (let ((reduced (map (lambda (pair)
                        `(,(car pair) .
                          ,(if (cdr pair)
                               (delete process (cdr pair)) #f)))
                      dependencies)))
    ;; Set the empty pairs to #f, so that it is equivalant to
    ;; a missing key.
    (map (lambda (pair)
           (let ((pair-dependencies (cdr pair)))
             `(,(car pair) . ,(if (null? pair-dependencies)
                                  #f
                                  pair-dependencies))))
         reduced)))

(define (resolve-dependencies processes dependencies)
  "Removes PROCESSES from the list of DEPENDENCIES."
  (if (null? processes)
      dependencies
      (resolve-dependencies
       (cdr processes)
       (resolve-dependency (car processes) dependencies))))

(define (full-dependency-list processes dependencies)
  "Returns a list of dependency pairs, including implicit dependency pairs."
  (map (lambda (process)
         `(,process . ,(assoc-ref dependencies process)))
       processes))

(define (compute-free-points processes dependencies)
  "Returns a list of processes that can run immediately."
  (map car (remove list? (full-dependency-list processes dependencies))))

(define* (sequential-execution-order processes dependencies #:optional (order '()))
  "Returns the list of PROCESSES, re-ordered so it can be executed in a sequence
and adhere to the dependencies provided in DEPENDENCIES."
  (if (null? processes)
    (reverse order)
    (let* ((resolvable (compute-free-points processes dependencies))
           (reduced-dependencies (resolve-dependencies resolvable dependencies))
           (leftovers (lset-difference eq? processes resolvable)))
      (if (null? resolvable)
          #f
          (sequential-execution-order
           leftovers
           (resolve-dependencies resolvable dependencies)
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
              (stepper leftovers
                       (resolve-dependencies resolvable dependencies)
                       (append (list resolvable) order))))))

  (stepper processes dependencies '()))

(define* (graph-execution-order processes dependencies #:optional (order '()))
  "Returns the list of PROCESSES, ordered such that each list contains a
sequential output of a branch in the dependency graph defined by
DEPENDENCIES."
  
  )

