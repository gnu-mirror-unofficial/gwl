;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Roel Janssen <roel@gnu.org>
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

(define-module (gnu workflows)
  #:use-module (gnu packages)
  #:use-module (guix utils)
  #:use-module (guix combinators)
  #:use-module (guix workflows)
  #:use-module (guix discovery)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%workflow-module-path
            all-workflow-modules
            fold-workflows
            find-workflows
            find-workflow-by-name
            find-workflow-by-full-name))

(define %workflow-module-path
  ;; Search path for process modules.  Each item must be either a directory
  ;; name or a pair whose car is a directory and whose cdr is a sub-directory
  ;; to narrow the search.
  (let* ((not-colon   (char-set-complement (char-set #\:)))
         (environment (string-tokenize (or (getenv "GUIX_WORKFLOW_PATH") "")
                                       not-colon)))
    (for-each (lambda (directory)
                (set! %load-path (cons directory %load-path))
                (set! %load-compiled-path
                      (cons directory %load-compiled-path)))
              environment)
    (make-parameter environment)))

(define* (all-workflow-modules #:optional (path (%workflow-module-path)))
  "Return the list of workflow modules found in PATH, a list of directories to
search."
  (fold-right (lambda (spec result)
                (match spec
                  ((? string? directory)
                   (append (scheme-modules directory) result))
                  ((directory . sub-directory)
                   (append (scheme-modules directory sub-directory)
                           result))))
              '()
              path))

(define (fold-workflows proc init)
  "Call (PROC WORKFLOW RESULT) for each available workflow, using INIT as
the initial value of RESULT.  It is guaranteed to never traverse the
same workflow twice."
  (identity   ; discard second return value
   (fold2 (lambda (module result seen)
            (fold2 (lambda (var result seen)
                     (if (and (workflow? var)
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

(define find-workflow-by-name
  (let ((workflows (delay
                     (fold-workflows (lambda (p r)
                                       (vhash-cons (workflow-name p) p r))
                                     vlist-null)))
        (version>? (lambda (p1 p2)
                     (version>? (workflow-version p1) (workflow-version p2)))))
    (lambda* (name #:optional version)
      "Return the list of workflows with the given NAME.  If VERSION is not #f,
then only return workflows whose version is prefixed by VERSION, sorted in
decreasing version order."
      (let ((matching (sort (vhash-fold* cons '() name (force workflows))
                            version>?)))
        (if version
            (filter (lambda (workflow)
                      (string-prefix? version (workflow-version workflow)))
                    matching)
            matching)))))

(define find-workflow-by-full-name
  (let ((workflows (delay
                     (fold-workflows (lambda (p r)
                                       (vhash-cons (workflow-full-name p) p r))
                                     vlist-null)))
        (version>? (lambda (p1 p2)
                     (version>? (workflow-version p1) (workflow-version p2)))))
    (lambda* (name #:optional version)
      "Return the list of workflows with the given NAME.  If VERSION is not #f,
then only return workflows whose version is prefixed by VERSION, sorted in
decreasing version order."
      (let ((matching (sort (vhash-fold* cons '() name (force workflows))
                            version>?)))
        (if version
            (filter (lambda (workflow)
                      (string-prefix? version (workflow-version workflow)))
                    matching)
            matching)))))

(define find-workflows
  (let ((workflows (delay
                     (fold-workflows (lambda (p r)
                                       (vhash-cons (workflow-name p) p r))
                                     vlist-null)))
        (version>? (lambda (p1 p2)
                     (version>? (workflow-version p1) (workflow-version p2)))))
    (lambda* (keyword)
      "Return the list of workflows with the given NAME.  If VERSION is not #f,
then only return workflows whose version is prefixed by VERSION, sorted in
decreasing version order."
      (let ((wfs (force workflows)))
        (if (null? wfs)
            '()
            (vlist-filter
             (lambda (item)
               (let ((wf (cdr item)))
                 (or (string-contains-ci (workflow-full-name wf) keyword)
                     (string-contains-ci (workflow-synopsis wf) keyword)
                     (string-contains-ci (workflow-description wf) keyword))))
             wfs))))))
