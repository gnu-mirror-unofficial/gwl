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

(define-module (guix workflows)
  #:use-module (guix processes)
  #:use-module (guix packages)
  #:use-module (guix workflows execution-order)
  #:use-module (guix workflows utils)
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
            workflow-run

            workflow->dot))

;;; ---------------------------------------------------------------------------
;;; RECORD TYPES
;;; ---------------------------------------------------------------------------

(define-record-type* <workflow>
  workflow make-workflow
  workflow?

  ;; Basic information about the workflow
  (name workflow-name)
  (version workflow-version)
  (synopsis workflow-synopsis)
  (description workflow-description)

  ;; The input and output of a workfow will be passed to each starting process.
  ;; This can be files or directories, depending on what the workflow expects.
  (input workflow-input)
  (output workflow-output)

  ;; Processes are functions taking two parameters (input and output) that
  ;; return a <process> record type.
  (processes workflow-processes)

  ;; Processes can depend on each other.  By defining dependency pairs
  ;; in the form (A B) where A must be executed after B.
  ;;
  ;; When automatic-restrictions is set to #t, we do not need to specify
  ;; workflow-restrictions manually.  The dependencies will be computed
  ;; automatically.
  ;;
  ;; Automatically computing the dependencies may be too simplistic for
  ;; your workflow.  You can therefore switch to specifying manual
  ;; restrictions too.
  (restrictions workflow-restrictions (default '()))
  (automatic-restrictions workflow-automatic-restrictions
                          (default #f))

  ;; Arguments are literally command-line arguments that can be passed
  ;; when executing a specific workflow.  This allows users to turn features
  ;; off and on, and pass an input and output directory.
  ;;
  ;; The arguments are passed as a list to the workflow record.
  (arguments workflow-arguments (default #f))

  ;; When a workflow requires additional code to execute, it can be
  ;; specified in the following field.
  (execution workflow-execution (default #f)))

(define (workflow-full-name arg)
  "Writes the name and version as a single string of PROCESS to PORT."
  (string-append (workflow-name arg) "-" (workflow-version arg)))

(define (print-workflow workflow port)
  "Write a decent human-representation of a workflow of WORKFLOW to PORT."
  (simple-format port "#<workflow ~a>" (workflow-full-name workflow)))

(define (print-workflow-record workflow port)
  "Write a multi-line representation of PROC to PORT."
  (match workflow
    (($ <workflow> name version synopsis description)
     (format port (string-append "name: ~a~%version: ~a~%synopsis: ~a~%"
                                 "description: ~a~%processes: ~{~%  * ~a~}~%")
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

;; TODO: Job dependencies can be 
(define* (workflow-run workflow engine #:key (parallel? #t))
  "Runs WORKFLOW using ENGINE."
  (let ((order (workflow-run-order workflow #:parallel? parallel?)))
    (format #t "# Please run the following:~%~%")
    (for-each (lambda (process)
                (process->script process engine #:stand-alone? #f))
              (list-ref order 0))))

;;; ---------------------------------------------------------------------------
;;; GRAPHING FUNCTIONALITY
;;; ---------------------------------------------------------------------------

(define take-color (color-scheme-stepper %modern-color-scheme))

(define (workflow-dot-prettify-node process)
  "Returns a string of prettified node names for a Graphviz graph."
  (let* ((proc process)
         (pretty-name (string-map (lambda (x)
                                    (if (eq? x #\-) #\  x))
                                  (process-name proc))))
    (format #f (string-append " ~s [shape=box,style=\"rounded,filled\",fillcolo"
                              "r=~s,label=<<FONT POINT-SIZE=\"14\"><U>~a</U></F"
                              "ONT><BR/><FONT POINT-SIZE=\"12\">~a<BR/><BR/>Use"
                              "s: ~{~a~^, ~}.</FONT>>];~%")
            (process-full-name proc)
            (take-color)
            (string-upcase pretty-name)
            (process-synopsis proc)
            (if (process-package-inputs proc)
                (map (lambda (pair)
                       (package-full-name (cadr pair)))
                     (process-package-inputs proc))
                '("-")))))

(define* (workflow->dot workflow #:key (parallel? #t))
  "Returns the workflow's processes formatted in Graphviz's Dot language as a
directed acyclic graph."
  (format #f "digraph G {~%  graph [bgcolor=transparent, fontsize=24];~%~{~a~}~%~{~a~}}"
          ;; (string-upcase (string-map (lambda (x)
          ;;                              (if (eq? x #\-) #\  x))
          ;;                            (workflow-name workflow)))
          (map workflow-dot-prettify-node (workflow-processes workflow))
          (let ((restrictions (workflow-restrictions workflow)))
            (if (not restrictions)
                '("")
                (map (lambda (pair)
                       (let ((first (list-ref pair 0))
                             (second (list-ref pair 1)))
                         (format #f "  ~s -> ~s~%"
                                 ;; Reversed order here because the flow is exactly
                                 ;; the opposite of the dependency chain.
                                 (process-full-name second)
                                 (process-full-name first))))
                     restrictions)))))
