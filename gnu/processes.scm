;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Roel Janssen <roel@gnu.org>
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

(define-module (gnu processes)
  #:use-module (gnu packages)
  #:use-module (gnu workflows)
  #:use-module (guix utils)
  #:use-module (guix combinators)
  #:use-module (guix processes)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (fold-processes
            find-processes
            find-process-by-name))

(define %distro-root-directory
 ;; Absolute file name of the module hierarchy.
 (dirname (search-path %load-path "guix.scm")))

(define (fold-processes proc init)
  "Call (PROC PROCESS RESULT) for each available process, using INIT as
the initial value of RESULT.  It is guaranteed to never traverse the
same process twice."
  (identity   ; discard second return value
   (fold2 (lambda (module result seen)
            (fold2 (lambda (var result seen)
                     (if (and (process? var)
                              (not (vhash-assq var seen)))
                         (values (proc var result)
                                 (vhash-consq var #t seen))
                         (values result seen)))
                   result
                   seen
                   (module-map (lambda (sym var)
                                 (false-if-exception (variable-ref var)))
                               module)))
          init
          vlist-null
          (all-workflow-modules))))

(define find-process-by-name
  (let ((processes (delay
                     (fold-processes (lambda (p r)
                                       (vhash-cons (process-name p) p r))
                                     vlist-null)))
        (version>? (lambda (p1 p2)
                     (version>? (process-version p1) (process-version p2)))))
    (lambda* (name #:optional version)
      "Return the list of processes with the given NAME.  If VERSION is not #f,
then only return processes whose version is prefixed by VERSION, sorted in
decreasing version order."
      (let ((matching (sort (vhash-fold* cons '() name (force processes))
                            version>?)))
        (if version
            (filter (lambda (process)
                      (string-prefix? version (process-version process)))
                    matching)
            matching)))))

(define find-processes
  (let ((processes (delay
                     (fold-processes (lambda (p r)
                                       (vhash-cons (process-name p) p r))
                                     vlist-null)))
        (version>? (lambda (p1 p2)
                     (version>? (process-version p1) (process-version p2)))))
    (lambda* (keyword)
      "Return the list of processes with the given NAME.  If VERSION is not #f,
then only return processes whose version is prefixed by VERSION, sorted in
decreasing version order."
      (let ((procs (force processes)))
        (if (null? procs)
            '()
            (vlist-filter
             (lambda (item)
               (let ((proc (cdr item)))
                 (or (string-contains-ci (process-name proc) keyword)
                     (string-contains-ci (process-synopsis proc) keyword)
                     (string-contains-ci (process-description proc) keyword))))
             procs))))))
