;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl workflows)
  #:use-module (gwl processes)
  #:use-module (gwl workflows execution-order)
  #:use-module (gwl workflows utils)
  #:use-module (guix records)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export (workflow
            workflow?
            workflow-name
            workflow-full-name
            workflow-version
            workflow-processes
            workflow-restrictions
            workflow-synopsis
            workflow-description

            print-workflow-record

            workflow-run-order
            workflow-prepare
            workflow-run

            graph))

;;; ---------------------------------------------------------------------------
;;; RECORD TYPES
;;; ---------------------------------------------------------------------------

(define-record-type* <workflow>
  workflow make-workflow
  workflow?

  ;; Basic information about the workflow
  (name workflow-name)
  (version workflow-version           (default ""))
  (synopsis workflow-synopsis         (default ""))
  (description workflow-description   (default ""))

  ;; Processes are values of the <process> record type.  This field
  ;; can hold either a plain list of processes or an adjacency list of
  ;; processes and their dependencies.
  (processes workflow-processes*)

  ;; This field is deprecated!
  ;;
  ;; The legacy way to specify process dependencies is by providing an
  ;; adjacency list of processes.
  (restrictions _workflow-restrictions (default #f)))

(define-syntax graph
  (lambda (x)
    (syntax-case x (->)
      ((_ (source -> targets ...) ...)
       #`(list (list source targets ...) ...)))))

;; This procedure would better be named workflow-processes->list, but
;; we keep the name for the benefit of existing workflows depending on
;; the behaviour of the old record accessor.
(define (workflow-processes workflow)
  "Return a list of all processes."
  (match (workflow-processes* workflow)
    (((and association (source target ...)) ...)
     (delete-duplicates
      (apply append association) eq?))
    (x x)))

(define (workflow-restrictions workflow)
  "Return process restrictions as an alist mapping processes to their
dependencies."
  (or (_workflow-restrictions workflow) ; legacy
      (match (workflow-processes*  workflow)
        (((and association (source target ...)) ...)
         association)
        (_ '()))))

(define (workflow-full-name arg)
  "Writes the name and version as a single string of PROCESS to PORT."
  (if (string= (workflow-version arg) "")
      (workflow-name arg)
      (string-append (workflow-name arg) "-" (workflow-version arg))))

(define (print-workflow workflow port)
  "Write a decent human-representation of a workflow of WORKFLOW to PORT."
  (simple-format port "#<workflow ~a>" (workflow-full-name workflow)))

(define* (print-workflow-record workflow #:optional (port #t))
  "Write a multi-line representation of PROC to PORT."
  (match workflow
    (($ <workflow> name version synopsis description)
     (format port "name: ~a~%version: ~a~%synopsis: ~a~%\
description: ~a~%processes: ~{~%  * ~a~}~%"
             name version synopsis description
             (map (lambda (proc)
                    (process-full-name proc))
                  (workflow-processes workflow))))))

(set-record-type-printer! <workflow> print-workflow)

;;; ---------------------------------------------------------------------------
;;; RUNNER FUNCTIONALITY
;;; ---------------------------------------------------------------------------

(define* (workflow-run-order workflow #:key (parallel? #t))
  "Returns a list of processes in WORKFLOW in the order in which the
processes can be executed.  When parallel? is set to #T, the list
contains lists of processes that can be executed in parallel."
  (let ((order-function (if parallel?
                            parallel-step-execution-order
                            sequential-execution-order)))
    (order-function (workflow-processes workflow)
                    (workflow-restrictions workflow))))

(define* (dispatch-workflow workflow proc #:key (parallel? #t))
  "Runs WORKFLOW by applying PROC to ordered processes."
  (define visit (if parallel?
                    (lambda (step)
                      (for-each (cut proc <> #:workflow workflow)
                                ;; By reversing the order of the processes
                                ;; in STEP we keep the output order the same
                                ;; as the order of the sequential function.
                                (reverse step)))
                    proc))
  (or (and=>
       (workflow-run-order workflow #:parallel? parallel?)
       (cut for-each visit <>))
      (format (current-error-port)
              "Error: Cannot determine process execution order.~%")))

(define* (workflow-prepare workflow engine #:key (parallel? #t))
  (dispatch-workflow workflow
                     (process->script engine)
                     #:parallel? parallel?))

(define* (workflow-run workflow engine #:key (parallel? #t))
  (dispatch-workflow workflow
                     (process->script->run engine)
                     #:parallel? parallel?))
