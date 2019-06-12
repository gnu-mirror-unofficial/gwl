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
  #:use-module (gwl oop)
  #:use-module (gwl processes)
  #:use-module (gwl process-engines)
  #:use-module (gwl workflows execution-order)
  #:use-module (gwl workflows utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (oop goops)
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

            workflow-free-inputs
            workflow-run-order
            workflow-prepare
            workflow-run

            graph
            auto-connect))

;;; ---------------------------------------------------------------------------
;;; RECORD TYPES
;;; ---------------------------------------------------------------------------

(define-class <workflow> (<gwl-class>)
  ;; Slots
  (name
   #:accessor workflow-name
   #:init-keyword #:name
   #:required? #t)
  (version
   #:accessor workflow-version
   #:init-keyword #:version
   #:init-value #f)
  (synopsis
   #:accessor workflow-synopsis
   #:init-keyword #:synopsis
   #:init-value "")
  (description
   #:accessor workflow-description
   #:init-keyword #:description
   #:init-value "")
  (processes
   #:accessor workflow-processes*
   #:init-keyword #:processes
   #:init-value '()
   #:implicit-list? #t
   #:validator? (lambda (value)
                  (and (list? value)
                       (every process? value))))
  (_restrictions)
  (restrictions
   #:accessor workflow-restrictions
   #:init-keyword #:restrictions
   #:init-value #f
   #:allocation #:virtual
   #:slot-ref
   (lambda (o)
     (or (slot-ref o '_restrictions)
         (match (slot-ref o 'processes)
           (((and association (source target ...)) ...)
            association)
           (_ '()))))
   #:slot-set!
   (lambda (o a)
     (when a
       (format (current-error-port)
               "workflow: The \"restrictions\" field is deprecated.  \
Use \"processes\" to specify process dependencies.~%"))
     (slot-set! o '_restrictions a)))
  ;; Class options
  #:name "workflow")

;; This procedure would better be named workflow-processes->list, but
;; we keep the name for the benefit of existing workflows depending on
;; the behaviour of the old record accessor.
;; TODO: integrate into class definition?
(define (workflow-processes workflow)
  "Return a list of all processes."
  (match (workflow-processes* workflow)
    (((and association (source target ...)) ...)
     (delete-duplicates
      (apply append association) eq?))
    (x x)))

(define (workflow? thing)
  (is-a? thing <workflow>))

;; This is a constructor for <workflow> instances.  It permits the use
;; of multiple field values (implicit lists) and cross-field
;; references.  It does not, however, validate any fields or their
;; values.
;; TODO: support "inherit" syntax.
;; TODO: merge with "process" macro
(define-syntax workflow
  (lambda (x)
    (syntax-case x ()
      ((_ fields ...)
       #`(let* (#,@(map (lambda (field)
                          (syntax-case field ()
                            ((empty-field)
                             (syntax-violation #f "workflow: Empty field" #'empty-field))
                            ((key value)
                             #'(key value))
                            ((key values ...)
                             #'(key (list values ...)))))
                        #'(fields ...)))
           (make <workflow>
             #,@(append-map (lambda (field)
                              (syntax-case field ()
                                ((name . rest)
                                 #`((symbol->keyword 'name) name))))
                            #'(fields ...))))))))

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
                 (filter-map (cut assoc-ref process-by-output <>)
                             (process-inputs process))))
         processes)))

(define (workflow-free-inputs workflow)
  "Return a list of processes and their free inputs, i.e. inputs that
are not provided by the outputs of any other process."
  (let ((processes (workflow-processes workflow)))
    (delete-duplicates
     (lset-difference equal?
                      (append-map process-inputs processes)
                      (append-map process-outputs processes)))))

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
             (map process-full-name
                  (workflow-processes workflow))))))

(define-method (write (workflow <workflow>) port)
  (print-workflow workflow port))

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
  "Print scripts to be run for WORKFLOW given ENGINE."
  (define ordered-processes
    (workflow-run-order workflow #:parallel? parallel?))
  (for-each (lambda (command)
              (display command) (newline))
            (reverse (fold (workflow-kons workflow (process->script engine))
                           '() ordered-processes))))

(define* (workflow-run workflow engine
                       #:key
                       (inputs '())
                       (parallel? #t)
                       dry-run?
                       force?)
  "Run the WORKFLOW with the given process ENGINE.  When PARALLEL? is
#T try to run independent processes in parallel.  When DRY-RUN? is #T
only display what would be done.  When FORCE? is #T ignore the cache.
INPUTS is a list of strings mapping the names of free workflow inputs
to existing files."
  (define inputs-map
    (match (map (lambda (value)
                  ;; A mapping is optional, so normalize it.
                  (if (string-contains value "=")
                      (string-split value #\=)
                      (list value value)))
                inputs)
      (() '())
      (mapping mapping)))
  (define (inputs-valid?)
    (let-values (((input-names input-files)
                  (match inputs-map
                    (() (values '() '()))
                    (_ (apply values
                              (apply zip inputs-map))))))
      (match (lset-difference equal?
                              (workflow-free-inputs workflow)
                              input-names)
        (()
         ;; verify input files
         (match (filter (negate file-exists?) input-files)
           (()
            ;; Link all mapped input files to their target locations
            ;; TODO: ensure that target directories exist.
            (unless (null? inputs-map)
              (for-each (match-lambda
                          ((target source)
                           (unless (file-exists? target)
                             (link source target))))
                        inputs-map))
            #t)
           (missing
            (format (current-error-port)
                    "Missing files: ~{~%  * ~a~}.~%"
                    missing)
            #f)))
        (missing
         ;; Try to find the files in the environment.
         ;; XXX Tell user that we pick the files from the current
         ;; working directory.
         ;; XXX These files would need to be mapped into the
         ;; container.
         (let* ((found (filter file-exists? missing))
               (really-missing (lset-difference equal? missing found)))
           (or (null? really-missing)
               (begin (format (current-error-port)
                              "Missing inputs: ~{~%  * ~a~}.~%Provide them with --input=NAME=FILE.~%"
                              really-missing)
                      #f)))))))
  (define ordered-processes
    (workflow-run-order workflow #:parallel? parallel?))
  (define (run)
    (let ((make-script (process->script engine))
          (runner (process-engine-runner engine)))
      (define process->cache-prefix
        (make-process->cache-prefix workflow
                                    inputs-map
                                    ordered-processes
                                    engine))
      (define cached?
        (if force?
            (const #f)
            (lambda (process)
              (and (not (null? (process-outputs process)))
                   (let ((cache-prefix (process->cache-prefix process)))
                     (every (lambda (out)
                              (file-exists?
                               (string-append cache-prefix out)))
                            (process-outputs process)))))))
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
                      (let ((retval (apply system* command)))
                        (unless (zero? retval)
                          (format (current-error-port)
                                  "Error: process `~a' failed with return value ~a.~%"
                                  (process-name process)
                                  retval)
                          (exit retval)))
                      ;; Wait before generated files are accessed.
                      ;; This may be needed for distributed file
                      ;; systems.
                      (usleep (%cache-delay))

                      ;; Link files to the cache.
                      (for-each (cut cache! <> cache-prefix)
                                (process-outputs process))))))))))
  (when (inputs-valid?)
    (fold (workflow-kons workflow (run))
          '() ordered-processes)))
