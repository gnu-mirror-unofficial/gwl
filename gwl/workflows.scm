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
  #:use-module (gwl cache)
  #:use-module (gwl processes)
  #:use-module (gwl process-engines)
  #:use-module (gwl workflows execution-order)
  #:use-module (gwl workflows utils)
  #:use-module (guix records)
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

            graph
            auto-connect))

;;; ---------------------------------------------------------------------------
;;; RECORD TYPES
;;; ---------------------------------------------------------------------------

(define-record-type* <workflow>
  workflow* make-workflow
  workflow?

  ;; Basic information about the workflow
  (name workflow-name)
  (version workflow-version           (default #f))
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

;; Taken from (guix deprecation)
(define (source-properties->location-string properties)
  "Return a human-friendly, GNU-standard representation of PROPERTIES, a
source property alist."
  (let ((file   (assq-ref properties 'filename))
        (line   (assq-ref properties 'line))
        (column (assq-ref properties 'column)))
    (if (and file line column)
        (format #f "~a:~a:~a" file (+ 1 line) column)
        ;; TODO: Translate
        "<unknown location>")))

(define-syntax workflow
  (lambda (x)
    (syntax-case x ()
      ((_ . fields)
       #`(workflow*
          #,@(begin (for-each (match-lambda
                                ;; Warn about deprecated fields
                                (('restrictions . _)
                                 (format (current-error-port)
                                         "~a: The \"restrictions\" field is deprecated.  Use \"processes\" instead.~%"
                                         (source-properties->location-string (syntax-source x))))
                                (_ #f))
                              (syntax->datum #'fields))
                    #'fields))))))

(define-syntax graph
  (lambda (x)
    (syntax-case x (->)
      ((_ (source -> targets ...) ...)
       #`(list (list source targets ...) ...)))))

(define (auto-connect . processes)
  "Return an association list mapping processes to processes they
depend on.  This is accomplished by matching a the inputs of a process
with the outputs of other processes."
  (let ((process-by-output
         (append-map (lambda (process)
                       (filter-map (lambda (out)
                                     (cons out process))
                                   (process-outputs process)))
                     processes)))
    (map (lambda (process)
           (cons process
                 (map (lambda (input)
                        (or (assoc-ref process-by-output input)
                            (error (format #f "~a: no process provides ~a~%"
                                           (process-full-name process)
                                           input))))
                      (process-data-inputs process))))
         processes)))

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

(define (workflow-full-name workflow)
  "Return the name and version of WORKFLOW as a string."
  (if (workflow-version workflow)
      (string-append (workflow-name workflow) "-"
                     (workflow-version workflow))
      (workflow-name workflow)))

(define (print-workflow workflow port)
  "Write a decent human-representation of a workflow of WORKFLOW to PORT."
  (simple-format port "#<workflow ~a>" (workflow-full-name workflow)))

(define* (print-workflow-record workflow #:optional (port #t))
  "Write a multi-line representation of WORKFLOW to PORT."
  (match workflow
    (($ <workflow> name version synopsis description)
     (format port "name: ~a~%version: ~a~%synopsis: ~a~%\
description: ~a~%processes: ~{~%  * ~a~}~%"
             name
             (or version "(none)")
             (if (string-null? synopsis)
                 "(none)" synopsis)
             (if (string-null? description)
                 "(none)" description)
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
    (or (order-function (workflow-processes workflow)
                        (workflow-restrictions workflow))
        (error (format (current-error-port)
                       "Error: Cannot determine process execution order.~%")))))

(define (workflow-kons workflow proc)
  "Construct a procedure from the single-argument procedure PROC that
can be used in a fold over the WORKFLOW's processes."
  (lambda (item acc)
    (match item
      ((? list?)
       (fold (lambda (process res)
               (cons (proc process #:workflow workflow) res))
             acc
             ;; By reversing the order of the processes
             ;; in STEP we keep the output order the same
             ;; as the order of the sequential function.
             (reverse item)))
      (_ (cons (proc item) acc)))))

(define* (workflow-prepare workflow engine #:key (parallel? #t))
  (for-each (lambda (command)
              (display command) (newline))
            (reverse (fold (workflow-kons workflow (process->script engine))
                           '()
                           (workflow-run-order workflow #:parallel? parallel?)))))

(define* (workflow-run workflow engine
                       #:key
                       (parallel? #t)
                       dry-run?
                       force?)
  "Run the WORKFLOW with the given process ENGINE.  When PARALLEL? is
#T try to run independent processes in parallel.  When DRY-RUN? is #T
only display what would be done.  When FORCE? is #T ignore the cache."
  (define ordered-processes
    (workflow-run-order workflow #:parallel? parallel?))
  (define process->cache-prefix
    (make-process->cache-prefix workflow
                                ordered-processes
                                engine))
  (define cached?
    (if force?
        (const #f)
        (lambda (process)
          (let ((cache-prefix (process->cache-prefix process)))
            (every (lambda (out)
                     (file-exists?
                      (string-append cache-prefix out)))
                   (process-outputs process))))))
  (define run
    (let ((make-script (process->script engine))
          (runner (process-engine-runner engine)))
      (lambda* (process #:key workflow)
        (let ((cache-prefix (process->cache-prefix process)))
          (if (cached? process)
              (if dry-run?
                  (format (current-error-port)
                          "Would skip process \"~a\" (cached at ~a).~%"
                          (process-name process)
                          cache-prefix)
                  (begin
                    (format (current-error-port)
                            "Skipping process \"~a\" (cached at ~a).~%"
                            (process-name process)
                            cache-prefix)
                    ;; TODO: mount the cache directory in the container
                    ;; if containerized.  Otherwise link files from
                    ;; cache to expected location.
                    (for-each (cut restore! <> cache-prefix)
                              (process-outputs process))))

              ;; Not cached: execute the process!
              (let ((command (append runner
                                     (list (make-script process #:workflow workflow)))))
                (if dry-run?
                    (format (current-error-port)
                            "Would execute: ~{~a ~}~%" command)
                    (begin
                      (format (current-error-port)
                              "Executing: ~{~a ~}~%" command)
                      (apply system* command)
                      ;; Wait before generated files are accessed.
                      ;; This may be needed for distributed file
                      ;; systems.
                      (usleep (%cache-delay))

                      ;; Link files to the cache.
                      (for-each (cut cache! <> cache-prefix)
                                (process-outputs process))))))))))
  (fold (workflow-kons workflow run)
        '() ordered-processes))
