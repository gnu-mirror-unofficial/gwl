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
  #:use-module (gwl process-engines)
  #:use-module ((guix derivations)
                #:select (derivation->output-path
                          build-derivations))
  #:use-module (guix gexp)
  #:use-module ((guix monads) #:select (mlet return))
  #:use-module (guix records)
  #:use-module ((guix store)
                #:select (open-connection
                          run-with-store
                          %store-monad))
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:export (process
            process?
            process-name
            process-full-name
            process-version
            process-package-inputs
            process-data-inputs
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

            process->derivation
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

            procedure->gexp

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

;;
;; A process is a stand-alone unit doing some work.  This is typically a
;; single program, configured through command-line options, given a certain
;; input, producing a certain output.
;;
(define-record-type* <process>
  process make-process
  process?

  (name             process-name)
  (version          process-version        (default ""))
  (synopsis         process-synopsis       (default ""))
  (description      process-description    (default ""))

  ;; Inputs can be packages, files, and settings.
  (package-inputs   process-package-inputs (default '()))
  (data-inputs      process-data-inputs    (default '()))

  ;; Outputs can be anything, but are mostly files (I guess).
  (output-path      process-output-path    (default #f))
  (outputs          process-outputs*       (default '()))

  (run-time         process-run-time       (default #f))
  (procedure        process-procedure))

(define (print-process process port)
  "Write a concise representation of PROCESS to PORT."
  (match process
    (($ <process> name version synopsis description run-time procedure)
     (simple-format port "#<process ~a>" (process-full-name process)))))


(define* (print-process-record process #:optional (port #t))
  "Write a multi-line representation of PROC to PORT."
  (match process
    (($ <process> name version synopsis description run-time procedure)
     (format port "name: ~a~%version: ~a~%synopsis: ~a~%description: ~a~%~%"
             name version synopsis description))))

(define (process-full-name proc)
  "Returns the name and version of PROC."
  (if (string= (process-version proc) "")
      (process-name proc)
      (string-append (process-name proc) "-" (process-version proc))))

(set-record-type-printer! <process> print-process)


;;
;; A `complexity' is a way of describing the run-time complexity of a
;; `process'.
;;
(define-record-type* <complexity>
  complexity make-complexity
  complexity?

  (threads complexity-threads (default 1))
  (space complexity-space (default #f))
  (time complexity-time (default #f)))

(define (print-complexity complexity port)
  "Write a concise representation of COMPLEXITY to PORT."
  (match complexity
    (($ <complexity> threads space time)
     (simple-format port "<#complexity ~a sec, ~a bytes, ~a threads>"
                    time space threads))))


;;; Support for embedding foreign language snippets

(define-record-type* <language>
  language
  make-language
  language?
  (name          language-name)          ; symbol
  (call          language-call))         ; procedure

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
    ("_GWL_PROCESS_DATA_INPUTS" .
     ,(string-join (process-data-inputs process)))
    ("_GWL_PROCESS_OUTPUT_PATH" .
     ,(or (process-output-path process) ""))
    ("_GWL_PROCESS_OUTPUTS" .
     ,(string-join (process-outputs process)))
    ;; TODO: does this make sense?
    ("_GWL_PROCESS_RUN_TIME" .
     ,(or (process-run-time process) ""))))

(define language-python
  (language
   (name 'python)
   (call (lambda (process code)
           #~(begin
               (for-each (lambda (pair)
                           (setenv (car pair) (cdr pair)))
                         '#$(process->env process))
               (system* "python" "-c" #$code))))))

(define language-r
  (language
   (name 'R)
   (call (lambda (process code)
           (let ((args (append-map (lambda (line)
                                     (list "-e" line))
                                   (filter (negate string-null?)
                                           (string-split code #\newline)))))
             #~(begin
                 (for-each (lambda (pair)
                             (setenv (car pair) (cdr pair)))
                           '#$(process->env process))
                 (apply system* "Rscript" '#$args)))))))

(define languages
  (list language-python
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
  "Transform the procedure of PROCESS to a G-expression or return the
plain S-expression."
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
  (match (process-data-inputs process)
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

(define %daemon-connection #f)
(define (open-or-reuse-connection)
  (unless %daemon-connection
    (set! %daemon-connection (open-connection)))
  %daemon-connection)

;;; ---------------------------------------------------------------------------
;;; DERIVATIONS AND SCRIPTS FUNCTIONS
;;; ---------------------------------------------------------------------------

(define* (derivation->script drv #:optional (build? #t))
  "Write the output of a derivation DRV to a file.  When BUILD? is
set to #f, it only returns the output path."
  (let ((store (open-or-reuse-connection)))
    (run-with-store store
      (mlet %store-monad ((drv drv))
        (when build? (build-derivations store (list drv)))
        (return (derivation->output-path drv))))))

(define* (process->derivation proc #:key (guile (default-guile)))
  (gexp->derivation (process-full-name proc)
                    (procedure->gexp proc)
                    #:guile-for-build guile
                    #:graft? #f))

(define (process->script engine)
  "Builds a procedure that builds a derivation of the process PROCESS
according to ENGINE and displays the commands a user needs to run."
  (let* ((command-prefix (process-engine-command-prefix engine))
         (derivation-builder (process-engine-derivation-builder engine))
         (restrictions-func (process-engine-restrictions-string engine)))
    (lambda* (process #:key workflow (port (current-output-port)))
      (unless (process? process)
        (error (format #f "This is not a process!~%")))
      (let ((output (derivation->script (derivation-builder process)))
            (restrictions (restrictions-func process workflow)))
        (format port "~@[~a ~]~@[~a ~]~a~%"
                command-prefix restrictions output)))))

(define (process->script->run engine)
  "Return a procedure that builds a derivation of PROCESS according to
ENGINE and runs the resulting script."
  (let ((make-script (process->script engine)))
    (lambda* (process #:key workflow)
      (system (make-script process
                           #:workflow workflow
                           #:port #f)))))

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
of #f to exclude it."
  (filter-map filter processes))

(define (processes-filter-by-name processes name)
  "Returns a list of PROCESSES whose name (partially) matches NAME."
  (filter-map (lambda (proc)
                (and (process? proc)
                     (string-contains (process-name proc) name)
                     proc))
              processes))
