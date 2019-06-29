;;; Copyright © 2017, 2018 Roel Janssen <roel@gnu.org>
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

(define-module (gwl processes)
  #:use-module (oop goops)
  #:use-module (gwl oop)
  #:use-module (gwl process-engines)
  #:use-module ((guix derivations)
                #:select (derivation->output-path
                          build-derivations))
  #:use-module ((guix packages)
                #:select (package?))
  #:use-module ((gnu packages)
                #:select (specification->package))
  #:use-module (guix gexp)
  #:use-module ((guix store)
                #:select (with-store))
  #:use-module ((guix modules)
                #:select (source-module-closure))
  #:use-module (gnu system file-systems)
  #:use-module (gnu build linux-container)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (process
            process?
            process-name
            process-full-name
            process-version
            process-packages
            process-inputs
            process-run-time
            process-procedure
            process-synopsis
            process-description

            process-outputs
            process-output-path
            process-takes-available
            print-process-record

            complexity
            complexity-space
            complexity-time
            complexity-threads

            process->script
            process->script->run

            ;; Convenience functions
            gigabytes
            megabytes
            kilobytes
            minutes
            hours

            process-space
            process-time
            process-threads

            define-dynamically

            processes-filter
            processes-filter-by-name

            code-snippet
            code-snippet?
            code-snippet-language
            code-snippet-arguments
            code-snippet-code

            procedure->gexp
            containerize

            ;; For the lack of a better place.
            derivation->script
            default-guile))

;;; Commentary:
;;;
;;; This module provides a high-level mechanism to define processes in a
;;; Guix-based distribution.

;;; ---------------------------------------------------------------------------
;;; RECORD TYPES
;;; ---------------------------------------------------------------------------

;; A `complexity' is a way of describing the run-time complexity of a
;; `process'.
(define-class <complexity> ()
  (threads
   #:init-value 1
   #:init-keyword #:threads
   #:accessor complexity-threads)
  (space
   #:init-value #f
   #:init-keyword #:space
   #:accessor complexity-space)
  (time
   #:init-value #f
   #:init-keyword #:time
   #:accessor complexity-time))

;; Convenient DSL-like constructor.
(define-syntax complexity
  (lambda (x)
    (syntax-case x ()
      ((_ fields ...)
       #`(let ()
           #,@(map (match-lambda
                     ((empty-field)
                      (syntax-violation #f "complexity: Empty field" empty-field))
                     ((field single-value)
                      #`(define #,(datum->syntax #'x field)
                          #,(datum->syntax #'x single-value))))
                   (syntax->datum #'(fields ...)))
           (make <complexity>
             #,@(append-map (match-lambda
                              ((name . rest)
                               (list (datum->syntax #'x (symbol->keyword name))
                                     (datum->syntax #'x name))))
                            (syntax->datum #'(fields ...)))))))))

(define (complexity? thing)
  (is-a? thing <complexity>))

(define (print-complexity complexity port)
  "Write a concise representation of COMPLEXITY to PORT."
  (simple-format port "<#complexity ~a sec, ~a bytes, ~a threads>"
                 (complexity-time complexity)
                 (complexity-space complexity)
                 (complexity-threads complexity)))

(define-method (write (complexity <complexity>) port)
  (print-complexity complexity port))


;; A process is a stand-alone unit doing some work.  This is typically a
;; single program, configured through command-line options, given a certain
;; input, producing a certain output.

(define-class <process> (<gwl-class>)
  ;; Slots
  (name
   #:accessor process-name
   #:init-keyword #:name
   #:required? #t)
  (version
   #:accessor process-version
   #:init-keyword #:version
   #:init-value #f)
  (synopsis
   #:accessor process-synopsis
   #:init-keyword #:synopsis
   #:init-value "")
  (description
   #:accessor process-description
   #:init-keyword #:description
   #:init-value "")
  (packages
   #:accessor process-packages
   #:init-keyword #:packages
   #:init-value '()
   #:implicit-list? #t
   #:validator? (lambda (value)
                  (every package? value))
   #:transformer
   ;; TODO: the instance name is not be available at this point, so we
   ;; can't report the process name here.  We should move the
   ;; transformers and validators to a point after initialization.
   (lambda (instance value)
     (map (match-lambda
            ((and (? string?) spec)
             (catch #t
               (lambda ()
                 (specification->package spec))
               (lambda _
                 (error (format #f "no such package: ~a~%" spec)))))
            ((and (? package?) pkg)
             pkg)
            (x
             (error (format #f "must provide package value or string: ~a~%" x))))
          value)))
  (inputs
   #:accessor process-inputs
   #:init-keyword #:inputs
   #:init-value '()
   #:implicit-list? #t)
  (output-path
   #:accessor process-output-path
   #:init-keyword #:output-path
   #:init-value #f)
  (outputs
   #:accessor process-outputs*
   #:init-keyword #:outputs
   #:init-value '()
   #:implicit-list? #t
   #:validator list?)
  (run-time
   #:accessor process-run-time
   #:init-keyword #:run-time
   #:init-value #f
   #:validator complexity?)
  (procedure
   #:accessor process-procedure
   #:init-keyword #:procedure
   #:required? #t)
  ;; Class options
  #:name "process")

(define (process? thing)
  (is-a? thing <process>))

;; This is a constructor for <process> instances.  It permits the use
;; of multiple field values (implicit lists) and cross-field
;; references.  It does not, however, validate any fields or their
;; values.
;; TODO: support "inherit" syntax.
(define-syntax process
  (lambda (x)
    (syntax-case x ()
      ((_ fields ...)
       ;; If the last field is a plain code snippet produced with
       ;; special syntax, add the field name.
       (let ((fields* (match (reverse #'(fields ...))
                        ((last-field before ___)
                         (match (syntax->datum last-field)
                           ;; Wisp rules let the code-snippet be
                           ;; wrapped in an extra pair of parens
                           ;; unless we start the line with a dot.
                           ((('code-snippet . _))
                            (cons #`(procedure #,@last-field)
                                  (reverse before)))
                           (('code-snippet . _)
                            (cons #`(procedure #,last-field)
                                  (reverse before)))
                           (_ #'(fields ...)))))))
         #`(let* (#,@(map (lambda (field)
                            (syntax-case field ()
                              ((empty-field)
                               (syntax-violation #f "process: Empty field" #'empty-field))
                              ((key value)
                               #'(key value))
                              ((key values ...)
                               #'(key (list values ...)))))
                          fields*))
             (make <process>
               #,@(append-map (lambda (field)
                                (syntax-case field ()
                                  ((name . rest)
                                   #`((symbol->keyword 'name) name))))
                              fields*))))))))

(define (print-process process port)
  "Write a concise representation of PROCESS to PORT."
  (simple-format port "#<process ~a>" (process-full-name process)))

(define* (print-process-record process #:optional (port #t))
  "Write a multi-line representation of PROCESS to PORT."
  (format port "name: ~a~%version: ~a~%synopsis: ~a~%description: ~a~%~%"
          (process-name process)
          (process-version process)
          (process-synopsis process)
          (process-description process)))

(define (process-full-name process)
  "Returns the name and version of PROCESS."
  (if (process-version process)
      (string-append (process-name process) "-"
                     (process-version process))
      (process-name process)))

(define-method (write (process <process>) port)
  (print-process process port))


;;; Support for embedding foreign language snippets
(define-class <language> ()
  (name
   #:init-keyword #:name
   #:accessor language-name) ; symbol
  (call
   #:init-keyword #:call
   #:accessor language-call)) ; procedure

(define (process->env process)
  "Return an alist of environment variable names to values of fields
of PROCESS."
  `(("_GWL_PROCESS_NAME" .
     ,(process-name process))
    ("_GWL_PROCESS_SYNOPSIS" .
     ,(or (process-synopsis process) ""))
    ("_GWL_PROCESS_DESCRIPTION" .
     ,(or (process-description process) ""))
    ;; TODO: this doesn't always make sense as data inputs could be
    ;; procedures.
    ("_GWL_PROCESS_INPUTS" .
     ,(string-join (remove keyword? (process-inputs process))))
    ("_GWL_PROCESS_OUTPUT_PATH" .
     ,(or (process-output-path process) ""))
    ("_GWL_PROCESS_OUTPUTS" .
     ,(string-join (remove keyword? (process-outputs process))))
    ("_GWL_PROCESS_COMPLEXITY_THREADS" .
     ,(or (and=> (process-threads process) number->string) ""))
    ("_GWL_PROCESS_COMPLEXITY_SPACE" .
     ,(or (and=> (process-space process) number->string) ""))
    ("_GWL_PROCESS_COMPLEXITY_TIME" .
     ,(or (and=> (process-time process) number->string) ""))))

(define language-python
  (make <language>
    #:name 'python
    #:call (lambda (process code)
             #~(begin
                 (for-each (lambda (pair)
                             (setenv (car pair) (cdr pair)))
                           '#$(process->env process))
                 (exit (zero? (system* "python" "-c" #$code)))))))

(define language-r
  (make <language>
    #:name 'R
    #:call (lambda (process code)
             (let ((args (append-map (lambda (line)
                                       (list "-e" line))
                                     (filter (negate string-null?)
                                             (string-split code #\newline)))))
               #~(begin
                   (for-each (lambda (pair)
                               (setenv (car pair) (cdr pair)))
                             '#$(process->env process))
                   (exit (zero? (apply system* "Rscript" '#$args))))))))

(define language-bash
  (make <language>
    #:name 'bash
    #:call (lambda (process code)
             #~(begin
                 (for-each (lambda (pair)
                             (setenv (car pair) (cdr pair)))
                           '#$(process->env process))
                 (exit (zero? (system* "bash" "-c" #$code)))))))

(define language-sh
  (make <language>
    #:name 'sh
    #:call (lambda (process code)
             #~(begin
                 (for-each (lambda (pair)
                             (setenv (car pair) (cdr pair)))
                           '#$(process->env process))
                 (exit (zero? (system* "/bin/sh"
                                       "-c" #$code)))))))

(define languages
  (list language-sh
        language-bash
        language-python
        language-r))


;;; ---------------------------------------------------------------------------
;;; Syntactic sugar
;;; ---------------------------------------------------------------------------

(define-record-type <code-snippet>
  (code-snippet language arguments code)
  code-snippet?
  (language  code-snippet-language)
  (arguments code-snippet-arguments)
  (code      code-snippet-code))

(define (procedure->gexp process)
  "Transform the procedure of PROCESS to a G-expression."
  (define (sanitize-path path)
    (string-join (delete ".." (string-split path #\/))
                 "/"))
  (match (process-procedure process)
    ((? gexp? g) g)
    ((? list? s) s)
    (($ <code-snippet> name arguments code)
     (let ((call (or (and=> (find (lambda (lang)
                                    (eq? name (language-name lang)))
                                  languages)
                            language-call)
                     ;; There is no pre-defined way to execute the
                     ;; snippet.  Use generic approach.
                     (lambda (process code)
                       #~(begin
                           (for-each (lambda (pair)
                                       (setenv (car pair) (cdr pair)))
                                     '#$(process->env process))
                           (apply system*
                                  (string-append (getenv "_GWL_PROFILE")
                                                 #$(sanitize-path (symbol->string name)))
                                  '#$(append arguments
                                             (list code))))))))
       (call process code)))
    (whatever (error (format #f "unsupported procedure: ~a\n" whatever)))))

(define* (containerize exp #:key inputs outputs)
  "Wrap EXP, an S-expression or G-expression, in a G-expression that
causes EXP to be run in a container where the provided INPUTS and
OUTPUTS are mapped."
  (let* ((output-dirs
          (delete-duplicates (map dirname outputs)))
         (input-mappings
          (map (lambda (location)
                 (file-system-mapping
                  (source location)
                  (target location)
                  (writable? #f)))
               (lset-difference string=?
                                inputs
                                output-dirs)))
         (output-mappings
          (map (lambda (dir)
                 (file-system-mapping
                  (source dir)
                  (target dir)
                  (writable? #t)))
               output-dirs))
         (specs
          (map (compose file-system->spec
                        file-system-mapping->bind-mount)
               (append input-mappings
                       output-mappings))))
    (with-imported-modules (source-module-closure
                            '((gnu build linux-container)
                              (gnu system file-systems)))
      #~(begin
          (use-modules (gnu build linux-container)
                       (gnu system file-systems))
          (let ((thunk (lambda () #$exp)))
            (if (getenv "GWL_CONTAINERIZE")
                (call-with-container (append %container-file-systems
                                             (map spec->file-system
                                                  '#$specs))
                  thunk)
                (thunk)))))))

;;; ---------------------------------------------------------------------------
;;; ADDITIONAL FUNCTIONS
;;; ---------------------------------------------------------------------------

(define (process-outputs proc)
  "Return the output location(s) of process PROC."
  (let* ((root (process-output-path proc))
         (mangle (if root
                     (cut string-append root "/" <>)
                     identity)))
    (map mangle (process-outputs* proc))))

(define (process-takes-available process)
  "Returns #T when the data inputs of the PROCESS exist."
  (match (process-inputs process)
    ((? list? inputs)
     (every file-exists? inputs))
    (_ #t)))

;;; ---------------------------------------------------------------------------
;;; HACKS AND DUPLICATED FUNCTIONS FROM GEXP.
;;; ---------------------------------------------------------------------------

(define (default-guile)
  ;; Lazily resolve 'guile-final'.  This module must not refer to (gnu …)
  ;; modules directly, to avoid circular dependencies, hence this hack.
  (module-ref (resolve-interface '(gnu packages commencement))
              'guile-final))

;;; ---------------------------------------------------------------------------
;;; DERIVATIONS AND SCRIPTS FUNCTIONS
;;; ---------------------------------------------------------------------------

(define (derivation->script drv)
  "Write the output of a derivation DRV to a file.  When BUILD? is
set to #f, it only returns the output path."
  (with-store store
    (build-derivations store (list drv))
    (derivation->output-path drv)))

(define (process->script engine)
  "Builds a procedure that builds a derivation of the process PROCESS
according to ENGINE and returns the commands a user needs to run."
  (let ((derivation-builder (process-engine-derivation-builder engine)))
    (lambda* (process #:key workflow)
      (unless (process? process)
        (error (format #f "This is not a process!~%")))
      (derivation->script
       (derivation-builder process
                           #:workflow workflow)))))

(define (process->script->run engine)
  "Return a procedure that builds a derivation of PROCESS according to
ENGINE and runs the resulting script."
  (let ((make-script (process->script engine))
        (runner (process-engine-runner engine)))
    (lambda* (process #:key workflow)
      (apply system* (append runner
                             (list (make-script process #:workflow workflow)))))))

;;; ---------------------------------------------------------------------------
;;; CONVENIENCE FUNCTIONS
;;; ---------------------------------------------------------------------------

(define (gigabytes number)
  (* number 1024 1024 1024))

(define (megabytes number)
  (* number 1024 1024))

(define (kilobytes number)
  (* number 1024))

(define (minutes number)
  (* number 60))

(define (hours number)
  (* number 3600))

;; TODO: don't use primitive-eval!
(define-syntax-rule
  (define-dynamically name value)
  (primitive-eval `(define-public ,name ,value)))

(define (process-space process)
  (and=> (process-run-time process) complexity-space))

(define (process-time process)
  (and=> (process-run-time process) complexity-time))

(define (process-threads process)
  (and=> (process-run-time process) complexity-threads))

(define (processes-filter processes filter)
  "Returns a list of PROCESSES after applying FILTER.  FILTER
is a function that takes a process and returns the process to include it
or #f to exclude it."
  (filter-map filter processes))

(define (processes-filter-by-name processes name)
  "Returns a list of PROCESSES whose name (partially) matches NAME."
  (filter-map (lambda (proc)
                (and (process? proc)
                     (string-contains (process-name proc) name)
                     proc))
              processes))
