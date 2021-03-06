;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018, 2019, 2020, 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gwl config)
  #:use-module (gwl cache)
  #:use-module (gwl errors)
  #:use-module (gwl oop)
  #:use-module (gwl packages)
  #:use-module (gwl processes)
  #:use-module (gwl process-engines)
  #:use-module (gwl workflows execution-order)
  #:use-module (gwl workflows utils)
  #:use-module (gwl ui)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module ((guix derivations)
                #:select (built-derivations
                          derivation-outputs
                          derivation-output-path))
  #:use-module ((guix ui)
                #:select (build-notifier))
  #:use-module ((guix scripts package)
                #:select (%package-default-options))
  #:use-module ((guix scripts build)
                #:select (set-build-options-from-command-line))
  #:use-module ((guix status)
                #:select (with-status-verbosity))

  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (oop goops)
  #:export (make-workflow
            workflow?
            workflow-name
            workflow-full-name
            workflow-version
            workflow-processes
            workflow-restrictions
            workflow-synopsis
            workflow-description

            workflow-kons

            print-workflow-record

            compute-workflow
            computed-workflow?
            computed-workflow-workflow
            computed-workflow-script-proc
            computed-workflow-ordered-processes
            computed-workflow-cache-prefix-proc

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
  (before
   #:accessor workflow-before
   #:init-keyword #:before
   #:init-value (const #true)
   #:validator procedure?)
  (after
   #:accessor workflow-after
   #:init-keyword #:after
   #:init-value (const #true)
   #:validator procedure?)
  (processes
   #:accessor workflow-processes*
   #:init-keyword #:processes
   #:init-value '()
   #:required? #true
   #:implicit-list? #t
   #:validator (lambda (value)
                 (let loop ((remaining value))
                   (match remaining
                     ((head . tail)
                      (and (if (list? head)
                               (every process? head)
                               (process? head))
                           (loop tail)))
                     (() #true)))))
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
(define-syntax make-workflow
  (lambda (x)
    (syntax-case x ()
      ((_ fields ...)
       #`(let* (#,@(map (lambda (field)
                          (syntax-case field ()
                            ((empty-field)
                             (syntax-violation #f "workflow: Empty field" #'empty-field))
                            ((key value)
                             ;; No valid fields are keywords.
                             ;; Signal this error early instead of
                             ;; constructing an invalid let*.
                             (when (keyword? (syntax->datum #'key))
                               (syntax-violation #f "workflow: Invalid field name" #'key))
                             #'(key value))
                            ((key values ...)
                             ;; No valid fields are keywords.
                             ;; Signal this error early instead of
                             ;; constructing an invalid let*.
                             (when (keyword? (syntax->datum #'key))
                               (syntax-violation #f "workflow: Invalid field name" #'key))
                             ;; XXX: This is a crude way to allow
                             ;; for definitions inside of field
                             ;; values.
                             (let ((definition?
                                     (lambda (token)
                                       (string-prefix? "define"
                                                       (symbol->string token)))))
                               (match (syntax->datum #'(values ...))
                                 ((((? definition? token) . _) . _)
                                  ;; Start a definition context
                                  #'(key (let context () (begin values ...))))
                                 (_ #'(key (list values ...))))))))
                        #'(fields ...)))
           (make <workflow>
             #,@(append-map (lambda (field)
                              (syntax-case field ()
                                ((name . rest)
                                 #`((symbol->keyword 'name) name))))
                            #'(fields ...))))))))


(define-immutable-record-type <computed-workflow>
  (make-computed-workflow workflow
                          name-proc
                          script-proc
                          ordered-processes
                          cache-prefix-proc)
  computed-workflow?
  (workflow           computed-workflow-workflow)           ; <workflow>
  (name-proc          computed-workflow-name-proc)          ; procedure taking a <process>
  (script-proc        computed-workflow-script-proc)        ; procedure taking a <process>
  (ordered-processes  computed-workflow-ordered-processes)  ; list (of lists) of <process>
  (cache-prefix-proc  computed-workflow-cache-prefix-proc)) ; procedure taking a <process>

(define* (compute-workflow workflow
                           #:key
                           engine
                           (inputs '())
                           (parallel? #true)
                           containerize?)
  "Return a <computed-workflow> record value."
  (define inputs-map (inputs->map inputs))
  (define inputs-map-with-extra-files
    (prepare-inputs workflow inputs-map))
  (define ordered-processes
    (workflow-run-order workflow #:parallel? parallel?))

  (define (scripts-by-process)
    (let ((h (make-hash-table))
          (procedure-table (make-hash-table)))
      (for-each (lambda (process)
                  (let* ((input-files (lset-intersection
                                       string=?
                                       (map second inputs-map-with-extra-files)
                                       (process-inputs process)))
                         (key (compile-procedure process)))

                    ;; Skip unnecessary work by looking up existing
                    ;; scripts by procedure.
                    (or (and=> (hash-ref procedure-table key)
                               (lambda (script)
                                 (hashq-set! h process script)))

                        ;; Otherwise compute the script object and
                        ;; record it for later.
                        (begin
                          (log-event 'debug
                                     (G_ "Computing script for process `~a'~%")
                                     (process-name process))
                          (let ((script (process->script process
                                                         #:containerize? containerize?
                                                         #:workflow workflow
                                                         #:input-files input-files)))
                            (hashq-set! h process script)
                            (hash-set! procedure-table key script))))))
                (workflow-processes workflow))
      h))
  (define (wrapper-scripts-by-process scripts-table)
    (let ((make-wrapper (process-engine-wrapper engine)))
      (if make-wrapper
          (let ((h (make-hash-table)))
            (for-each (lambda (process)
                        (log-event 'debug
                                   (G_ "Computing wrapper script for process `~a'~%")
                                   (process-name process))
                        (hashq-set! h process
                                    (process->script-wrapper process
                                                             #:make-wrapper make-wrapper
                                                             #:workflow workflow
                                                             #:scripts-table scripts-table)))
                      (workflow-processes workflow))
            h)
          scripts-table)))
  (define (script-files-by-process scripts-table)
    (let* ((h (make-hash-table))
           (processes (workflow-processes workflow))
           (scripts (map-in-order (lambda (process)
                                    (hashq-ref scripts-table process))
                                  processes))
           (file-names
            (parameterize ((%guile-for-build (default-guile-derivation)))
              (log-event 'debug
                         (G_ "Generating all scripts and their dependencies.~%"))
              (with-status-verbosity (%config 'verbosity)
                (set-build-options-from-command-line
                 (inferior-store) %package-default-options)
                (with-build-handler (build-notifier #:verbosity (%config 'verbosity))
                  (run-with-store (inferior-store)
                    (mlet* %store-monad
                        ((drvs (mapm/accumulate-builds lower-object scripts))
                         (%    (built-derivations drvs)))
                      (return
                       (map (match-lambda
                              (((_ . output) . rest)
                               (derivation-output-path output)))
                            (map derivation-outputs drvs))))))))))
      (for-each (lambda (process script-file)
                  (hashq-set! h process script-file))
                processes
                file-names)
      h))
  (define plain-scripts-table
    (scripts-by-process))
  (define plain-script-files-table
    (script-files-by-process plain-scripts-table))
  (define wrapper-scripts-table
    (wrapper-scripts-by-process plain-scripts-table))
  (define wrapper-script-files-table
    (script-files-by-process wrapper-scripts-table))
  (define (script-for-process process)
    (hashq-ref wrapper-script-files-table process))

  (define process->cache-prefix
    (make-process->cache-prefix workflow
                                inputs-map-with-extra-files
                                ordered-processes
                                wrapper-script-files-table))

  (define (process->job-name process)
    "Returns a valid job name for PROCESS."
    (define sanitize-job-name
      ;; These characters are not okay for SGE job names.
      (let ((bad-chars (char-set #\/ #\: #\@ #\\ #\* #\?)))
        (lambda (name)
          (string-map
           (lambda (x)
             (if (char-set-contains? bad-chars x) #\- x)) name))))
    (string-append "gwl-"
                   (sanitize-job-name
                    (format #false "~a-~a"
                            (process-full-name process)
                            (process->hash-string process
                                                  plain-scripts-table)))))

  (make-computed-workflow workflow
                          process->job-name
                          script-for-process
                          ordered-processes
                          process->cache-prefix))


(define-syntax graph
  (lambda (x)
    (syntax-case x (->)
      ((_ (source -> targets ...) ...)
       #`(list (list source targets ...) ...)))))

(define (auto-connect . processes)
  "Return an association list mapping processes to processes they
depend on.  This is accomplished by matching the inputs of a process
with the outputs of other processes."
  ;; Normalize the arguments.  Ensure that this is just a simple list
  ;; of processes, not a nested list.  This makes for nicer syntax as
  ;; users can avoid "apply".
  (let ((processes*
         (letrec ((flatten (lambda (sub)
                             (append-map
                              (match-lambda
                                ((? list? l) (flatten l))
                                (x (list x)))
                              sub))))
           (flatten processes))))
    (let ((process-by-output
           (append-map (lambda (process)
                         (filter-map (lambda (out)
                                       (cons out process))
                                     (process-outputs process)))
                       processes*)))
      (map (lambda (process)
             (cons process
                   (filter-map (cut assoc-ref process-by-output <>)
                               (process-inputs process))))
           processes*))))

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
  "Write a decent human-representation of WORKFLOW to PORT."
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
processes can be executed.  When PARALLEL? is set to #T, the list
contains lists of processes that can be executed in parallel."
  (let ((order-function (if parallel?
                            parallel-step-execution-order
                            sequential-execution-order)))
    (or (order-function (workflow-processes workflow)
                        (workflow-restrictions workflow))
        (raise (condition
                (&gwl-error)
                (&message
                 (message "Cannot determine process execution order.")))))))

(define (workflow-kons proc)
  "Construct a procedure from the single-argument procedure PROC that
can be used in a fold over a WORKFLOW's processes."
  (lambda (item acc)
    (match item
      ((? list?)
       (fold (lambda (process res)
               (cons (proc process) res))
             acc
             ;; By reversing the order of the processes
             ;; in STEP we keep the output order the same
             ;; as the order of the sequential function.
             (reverse item)))
      (_ (cons (proc item) acc)))))

(define* (workflow-prepare workflow engine
                           #:key
                           (inputs '())
                           (parallel? #t)
                           containerize?)
  "Print scripts to be run for WORKFLOW given ENGINE."
  (define computed-workflow
    (begin
      (log-event 'info (G_ "Computing workflow `~a'...~%")
                 (workflow-name workflow))
      (compute-workflow workflow
                        #:engine engine
                        #:inputs inputs
                        #:parallel? parallel?
                        #:containerize? containerize?)))
  (define script-for-process
    (computed-workflow-script-proc computed-workflow))
  (for-each (lambda (command)
              (display command) (newline))
            (delete-duplicates
             (reverse (fold (workflow-kons script-for-process)
                            '()
                            (computed-workflow-ordered-processes computed-workflow)))
             string=?)))

(define (inputs->map inputs)
  "Given a list of strings INPUTS of the format \"a=b\" or just \"a\",
return a normalized mapping as a list of two element lists containing
\"a\" and \"b\" or just \"a\" and \"a\"."
  (map (lambda (value)
         ;; A mapping is optional, so normalize it.
         (if (string-contains value "=")
             (string-split value #\=)
             (list value value)))
       inputs))

(define (prepare-inputs workflow inputs-map)
  "Ensure that all files in the INPUTS-MAP alist exist and are linked
to the expected locations.  Pick unspecified inputs from the
environment.  Return either the INPUTS-MAP alist with any additionally
used input file names added, or raise a condition containing the list
of missing files."
  (define-values (input-names input-files)
    (match inputs-map
      (() (values '() '()))
      (_ (apply values
                (apply zip inputs-map)))))
  (define unspecified-inputs
    (lset-difference equal?
                     (workflow-free-inputs workflow)
                     input-names))

  ;; TODO: can these two cases be merged?
  (match unspecified-inputs
    (()
     ;; verify input files
     (match (remove file-exists? input-files)
       (()
        ;; Link all mapped input files to their target locations
        ;; TODO: ensure that target directories exist.
        (for-each (match-lambda
                    ((target source)
                     (unless (file-exists? target)
                       (link source target))))
                  inputs-map)
        inputs-map)
       (missing
        (raise (condition
                (&missing-inputs (files missing)))))))
    (missing
     ;; Try to find the files in the environment.
     (let* ((found (filter file-exists? missing))
            (really-missing (lset-difference equal? missing found)))
       (if (null? really-missing)
           (append inputs-map
                   (map (lambda (file) (list file file))
                        found))
           (raise (condition
                   (&missing-inputs (files really-missing)))))))))

(define* (workflow-run workflow engine
                       #:key
                       (inputs '())
                       (parallel? #t)
                       containerize?
                       dry-run?
                       force?)
  "Run the WORKFLOW with the given process ENGINE.  When PARALLEL? is
#T try to run independent processes in parallel.  When DRY-RUN? is #T
only display what would be done.  When FORCE? is #T ignore the cache.
INPUTS is a list of strings mapping the names of free workflow inputs
to existing files.

When CONTAINERIZE? is #T build a process script that spawns a
container."
  (define computed-workflow
    (begin
      (log-event 'info (G_ "Computing workflow `~a'...~%")
                 (workflow-name workflow))
      (compute-workflow workflow
                        #:engine engine
                        #:inputs inputs
                        #:parallel? parallel?
                        #:containerize? containerize?)))
  (define runner (process-engine-runner engine))
  (define process->cache-prefix
    (computed-workflow-cache-prefix-proc computed-workflow))
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
  (define script-for-process
    (computed-workflow-script-proc computed-workflow))

  (define process->job-name
    (computed-workflow-name-proc computed-workflow))

  (define* (run-local #:key built-script process command cache-prefix job-name)
    (let* ((status   (run-process-command command))
           (signal   (status:term-sig status))
           (exit-val (status:exit-val status)))
      (when signal
        (log-event 'error
                   (G_ "process `~a' termined with signal ~a.~%")
                   (process-name process)
                   signal)
        (exit exit-val))
      (unless (zero? exit-val)
        (log-event 'error
                   (G_ "process `~a' failed with return value ~a.~%")
                   (process-name process)
                   exit-val)
        (exit exit-val)))
    ;; Wait before generated files are accessed.
    ;; This may be needed for distributed file
    ;; systems.
    (usleep (%cache-delay))

    ;; Abort if declared outputs are missing.
    (for-each (lambda (output)
                (let ((canonical-name (if (absolute-file-name? output)
                                          output
                                          (string-append (getcwd) "/" output))))
                  (unless (file-exists? canonical-name)
                    (log-event 'error
                               (G_ "process `~a' failed to produce output ~a.~%")
                               (process-name process)
                               output)
                    (exit 1))))
              (process-outputs process))

    ;; Link files to the cache.
    (for-each (cut cache! <> cache-prefix)
              (process-outputs process)))

  (define* (execution-wrapper process #:optional (run run-local))
    "Use the procedure RUN to launch the script for PROCESS.  This
procedure wraps the actual runner by adding logging and cache checks."
    (let ((cache-prefix (process->cache-prefix process)))
      (if (cached? process)
          (if dry-run?
              (begin
                (log-event 'execute
                           (G_ "Would skip process \"~a\" (cached at ~a).~%")
                           (process-name process)
                           cache-prefix)
                #false)
              (begin
                (log-event 'execute
                           (G_ "Skipping process \"~a\" (cached at ~a).~%")
                           (process-name process)
                           cache-prefix)
                ;; TODO: mount the cache directory in the container
                ;; if containerized.  Otherwise link files from
                ;; cache to expected location.
                (for-each (cut restore! <> cache-prefix)
                          (process-outputs process))
                #false))

          ;; Not cached: execute the process!
          (let* ((built-script (script-for-process process))
                 (command (runner (list built-script
                                        (format #false "'~S'"
                                                (process->script-arguments process))))))
            (if dry-run?
                (begin
                  (log-event 'execute
                             (G_ "Would execute: ~{~a ~}~%") command)
                  #false)
                (begin
                  (log-event 'execute
                             (G_ "Executing: ~{~a ~}~%") command)
                  (run #:built-script built-script
                       #:process process
                       #:command command
                       #:cache-prefix cache-prefix
                       #:job-name (process->job-name process))))))))

  ;; Ensure that the environment for the selected process engine is
  ;; fine before doing any work.
  ((process-engine-check engine))

  ((workflow-before workflow))
  ((process-engine-run engine)
   #:wrap execution-wrapper
   #:processes (computed-workflow-ordered-processes computed-workflow))
  ((workflow-after workflow)))
