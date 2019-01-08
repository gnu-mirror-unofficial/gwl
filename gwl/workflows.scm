;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
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

(define-module (gwl workflows)
  #:use-module (gwl processes)
  #:use-module (gwl workflows execution-order)
  #:use-module (gwl workflows utils)
  #:use-module (guix records)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9 gnu)
  #:export (workflow
            workflow?
            workflow-name
            workflow-full-name
            workflow-version
            workflow-input
            workflow-output
            workflow-processes
            workflow-restrictions
            workflow-arguments
            workflow-synopsis
            workflow-description

            print-workflow-record

            workflow-run-order
            workflow-prepare
            workflow-run))

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

  ;; The input and output of a workfow will be passed to each starting process.
  ;; This can be files or directories, depending on what the workflow expects.
  (input workflow-input               (default #f))
  (output workflow-output             (default #f))

  ;; Processes are functions taking two parameters (input and output) that
  ;; return a <process> record type.
  (processes workflow-processes)

  ;; Processes can depend on each other.  By defining dependency pairs
  ;; in the form (A B) where A must be executed after B.
  (restrictions workflow-restrictions (default '()))

  ;; Arguments are literally command-line arguments that can be passed
  ;; when executing a specific workflow.  This allows users to turn features
  ;; off and on, and pass an input and output directory.
  ;;
  ;; The arguments are passed as a list to the workflow record.
  (arguments workflow-arguments       (default #f))

  ;; When a workflow requires additional code to execute, it can be
  ;; specified in the following field.
  (execution workflow-execution       (default #f)))

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
  "Returns a list of processes in WORKFLOW in the order in which the processes
can be executed.  When parallel? is set to #t, the list contains lists of
processes that can be executed in parallel."
  (let ((order-function (if parallel?
                            parallel-step-execution-order
                            sequential-execution-order)))
    (order-function (workflow-processes workflow)
                    (workflow-restrictions workflow))))

(define* (fold-workflow-processes workflow engine function #:key (parallel? #t))
  "Runs WORKFLOW using ENGINE."
  (let ((order (workflow-run-order workflow #:parallel? parallel?)))
    (if (not order)
        (format (current-error-port)
                "Error: Cannot determine process execution order.~%")
        (if parallel?
            (for-each (lambda (step)
                        (for-each (lambda (process)
                                    (function process engine
                                              #:stand-alone? #f
                                              #:workflow workflow))
                                  ;; By reversing the order of the processes in STEP
                                  ;; we keep the output order the same as the order
                                  ;; of the sequential function.
                                  (reverse step)))
                      order)
            (for-each (lambda (process)
                        (function process engine))
                      order)))))

(define* (workflow-prepare workflow engine #:key (parallel? #t))
  (fold-workflow-processes workflow engine process->script
                           #:parallel? parallel?))

(define* (workflow-run workflow engine #:key (parallel? #t))
  (fold-workflow-processes workflow engine process->script->run
                           #:parallel? parallel?))
