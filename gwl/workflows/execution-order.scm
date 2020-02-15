;;; Copyright © 2016, 2018 Roel Janssen <roel@gnu.org>
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

(define-module (gwl workflows execution-order)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (sequential-execution-order
            parallel-step-execution-order))

(define (find-roots nodes dependencies)
  "Return the roots of the graph spanned by NODES and the adjacency
list DEPENDENCIES.  As a second value, return the other NODES and a
new adjacency list without them."
  (let ((roots (fold (lambda (node roots)
                       (match (assoc-ref dependencies node)
                         ((or #f ()) (cons node roots))
                         (_ roots)))
                     '() nodes)))
    (values roots
            (lset-difference eq? nodes roots)
            (map
             (match-lambda
               ((source . targets)
                (cons source (lset-difference eq? targets roots))))
             (lset-difference eq? dependencies roots)))))

(define (parallel-step-execution-order processes dependencies)
  "Returns the list of PROCESSES grouped by processes that can be
executed in parallel at each step and adhere to the dependencies
provided in DEPENDENCIES."
  (let loop ((processes processes)
             (dependencies dependencies)
             (result '()))
    (call-with-values
        (lambda ()
          (find-roots processes dependencies))
      (lambda (roots processes dependencies)
        (match roots
          (() (reverse result))
          (_  (loop processes dependencies
                    (cons roots result))))))))

(define (sequential-execution-order processes dependencies)
  "Returns the list of PROCESSES, re-ordered so it can be executed in a sequence
and adhere to the dependencies provided in DEPENDENCIES."
  (apply append (parallel-step-execution-order processes dependencies)))

(define* (graph-execution-order processes dependencies #:optional (order '()))
  "Returns the list of PROCESSES, ordered such that each list contains a
sequential output of a branch in the dependency graph defined by
DEPENDENCIES.")

