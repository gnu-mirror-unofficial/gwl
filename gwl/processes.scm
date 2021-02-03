;;; Copyright © 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gwl errors)
  #:use-module (gwl oop)
  #:use-module (gwl process-engines)
  #:use-module (gwl packages)
  #:use-module ((guix profiles)
                #:select
                (profile
                 manifest-search-paths
                 packages->manifest))
  #:use-module ((guix search-paths)
                #:select
                (search-path-specification->sexp))
  #:use-module (guix gexp)
  #:use-module ((guix modules)
                #:select (source-module-closure))
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (make-process
            process?
            process-name
            process-full-name
            process-version
            process-packages
            process-raw-inputs
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
            process->script-arguments

            ;; Convenience functions
            kibibytes
            mebibytes
            gibibytes

            KiB
            MiB
            GiB

            seconds
            minutes
            hours

            process-space
            process-time
            process-threads

            processes-filter
            processes-filter-by-name

            code-snippet
            code-snippet?
            code-snippet-language
            code-snippet-arguments
            code-snippet-code

            procedure->gexp
            containerize))

;;; Commentary:
;;;
;;; This module provides a high-level mechanism to define processes in a
;;; Guix-based distribution.

;;; ---------------------------------------------------------------------------
;;; RECORD TYPES
;;; ---------------------------------------------------------------------------

;; A `complexity' is a way of describing the run-time complexity of a
;; `process'.
(define-class <complexity> (<gwl-class>)
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
       #`(let* (#,@(map (lambda (field)
                          (syntax-case field ()
                            ((empty-field)
                             (syntax-violation #f "complexity: Empty field" #'empty-field))
                            ((field n unit)
                             #'(field (unit n)))
                            ((field single-value)
                             #'(field single-value))))
                        #'(fields ...)))
           (make <complexity>
             #,@(append-map (lambda (field)
                              (syntax-case field ()
                                ((name . rest)
                                 #`((symbol->keyword 'name) name))))
                            #'(fields ...))))))))

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
   #:implicit-concatenation? #t
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
                  (every valid-package? value))
   #:transformer
   ;; TODO: the instance name is not be available at this point, so we
   ;; can't report the process name here.  We should move the
   ;; transformers and validators to a point after initialization.
   (lambda (instance value)
     (map (match-lambda
            ((and (? string?) spec)
             (lookup-package spec))
            ((and (? valid-package?) pkg)
             pkg)
            (x
             (raise
              (condition
               (&gwl-type-error
                (expected-type (list "<package>" "<inferior-package>" "<string>"))
                (actual-value x))))))
          value)))
  (inputs
   #:accessor process-raw-inputs
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

(define (flat-list . items)
  (fold (lambda (item acc)
          (match item
            ((? list? ls)
             (append acc ls))
            (_ (append acc (list item)))))
        '() items))

;; This is a constructor for <process> instances.  It permits the use
;; of multiple field values (implicit lists) and cross-field
;; references.  It does not, however, validate any fields or their
;; values.
;; TODO: support "inherit" syntax.
(define-syntax make-process
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
                            (append (reverse before)
                                    (list #`(procedure #,@last-field))))
                           (('code-snippet . _)
                            (append (reverse before)
                                    (list #`(procedure #,last-field))))
                           (_ #'(fields ...)))))))
         #`(let* (#,@(map (lambda (field)
                            (syntax-case field ()
                              ((empty-field)
                               (syntax-violation #f "process: Empty field" #'empty-field))
                              ((key value)
                               ;; No valid fields are keywords.
                               ;; Signal this error early instead of
                               ;; constructing an invalid let*.
                               (when (keyword? (syntax->datum #'key))
                                 (syntax-violation #f "process: Invalid field name" #'key))
                               #'(key value))
                              ((key values ...)
                               ;; No valid fields are keywords.
                               ;; Signal this error early instead of
                               ;; constructing an invalid let*.
                               (when (keyword? (syntax->datum #'key))
                                 (syntax-violation #f "process: Invalid field name" #'key))

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
                                   (_ #'(key (flat-list values ...))))))))
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
  `(("_GWL_PROCESS_NAME" . name)
    ("_GWL_PROCESS_SYNOPSIS" .
     ,(or (process-synopsis process) ""))
    ("_GWL_PROCESS_DESCRIPTION" .
     ,(or (process-description process) ""))
    ("_GWL_PROCESS_INPUTS" . inputs)
    ("_GWL_PROCESS_OUTPUT_PATH" .
     ,(or (process-output-path process) ""))
    ("_GWL_PROCESS_OUTPUTS" . outputs)
    ("_GWL_PROCESS_COMPLEXITY_THREADS" .
     ,(or (and=> (process-threads process) number->string) ""))
    ("_GWL_PROCESS_COMPLEXITY_SPACE" .
     ,(or (and=> (process-space process) number->string) ""))
    ("_GWL_PROCESS_COMPLEXITY_TIME" .
     ,(or (and=> (process-time process) number->string) ""))))

(define-syntax-rule (snippet-caller code-converter command)
  (lambda (process code)
    #~(begin
        (for-each (lambda (pair)
                    (setenv (car pair)
                            (let ((value (cdr pair)))
                              (if (symbol? value)
                                  (format #false "~a"
                                          (assoc-ref #{ %gwl process-arguments}# value))
                                  value))))
                  '#$(process->env process))
        (let ((retval (apply system*
                             (append command
                                     (code-converter (string-join (map (lambda (val)
                                                                         (if (list? val)
                                                                             (format #f "~{~a~^ ~}" val)
                                                                             (format #f "~a" val)))
                                                                       (list #$@code))
                                                                  ""))))))
          (or (zero? retval) (exit retval))))))

(define language-python
  (make <language>
    #:name 'python
    #:call (snippet-caller list '("python3" "-c"))))

(define language-r
  (make <language>
    #:name 'R
    #:call (snippet-caller
            (lambda (code)
              (append-map (lambda (line)
                            (list "-e" line))
                          (filter (negate string-null?)
                                  (string-split code #\newline))))
            '("Rscript"))))

(define language-bash
  (make <language>
    #:name 'bash
    #:call (snippet-caller list '("bash" "-c"))))

(define language-sh
  (make <language>
    #:name 'sh
    #:call (snippet-caller list '("/bin/sh" "-c"))))

(define languages
  (list language-sh
        language-bash
        language-python
        language-r))

(define-class <code-snippet> (<applicable-struct>)
  (language
   #:init-keyword #:language
   #:accessor code-snippet-language)
  (arguments
   #:init-keyword #:arguments
   #:accessor code-snippet-arguments)
  (code
   #:init-keyword #:code
   #:accessor code-snippet-code))

;; Code snippets should be evaluating to themselves for more
;; convenient Wisp use.
(define-method (initialize (self <code-snippet>) initargs)
  (next-method self
               (append (list #:procedure (lambda args self))
                       initargs)))

(define (code-snippet? thing)
  (is-a? thing <code-snippet>))

(define (code-snippet language arguments code)
  (make <code-snippet>
    #:language language
    #:arguments arguments
    #:code code))

(define (procedure->gexp process)
  "Transform the procedure of PROCESS to a G-expression."
  (define (sanitize-path path)
    (string-join (delete ".." (string-split path #\/))
                 "/"))
  (match (process-procedure process)
    ((? gexp? g) g)
    ((? list? s) s)
    ((? code-snippet? snippet)
     (let* ((name (code-snippet-language snippet))
            (arguments (code-snippet-arguments snippet))
            (code (code-snippet-code snippet))
            (call (or (and=> (find (lambda (lang)
                                     (eq? name (language-name lang)))
                                   languages)
                             language-call)
                      ;; There is no pre-defined way to execute the
                      ;; snippet.  Use generic approach.
                      (snippet-caller list
                                      (list
                                       (string-append (getenv "_GWL_PROFILE")
                                                      #$(sanitize-path (symbol->string name))))))))
       (call process code)))
    (whatever (error (format #f "unsupported procedure: ~a\n" whatever)))))


(define* (script-modules #:optional containerize?)
  (if containerize?
      '((gnu build accounts)
        (gnu build linux-container)
        (gnu system file-systems)
        (guix build utils)
        (guix search-paths))
      '((guix search-paths))))

(define* (containerize script #:key inputs outputs)
  "Call SCRIPT, a program-file object, in a G-expression that sets up
a container where the provided INPUTS are available (in addition to
all store items needed for execution).  OUTPUTS are copied outside of
the container."
  (with-imported-modules (source-module-closure
                          (script-modules 'container))
    #~(begin
        (use-modules (gnu build accounts)
                     (gnu build linux-container)
                     (gnu system file-systems)
                     (guix build utils))
        (define (location->file-system source target writable?)
          (file-system
            (device source)
            (mount-point target)
            (type "none")
            (flags (if writable?
                       '(bind-mount)
                       '(bind-mount read-only)))
            (check? #f)
            (create-mount-point? #t)))
        (let* ((pwd (getpw))
               (uid (getuid))
               (gid (getgid))
               (passwd (let ((pwd (getpwuid (getuid))))
                         (password-entry
                          (name (passwd:name pwd))
                          (real-name (passwd:gecos pwd))
                          (uid uid) (gid gid) (shell "/bin/sh")
                          (directory (passwd:dir pwd)))))
               (groups (list (group-entry (name "users") (gid gid))
                             (group-entry (gid 65534) ;the overflow GID
                                          (name "overflow")))))
          (call-with-container
              (append %container-file-systems
                      ;; Current directory for final outputs
                      (list (location->file-system
                             (canonicalize-path ".") "/gwl" #t))
                      (map (lambda (location)
                             (location->file-system location location #f))
                           (append (call-with-input-file #$(references-file script) read)
                                   '#$inputs)))
            (lambda ()
              (unless (file-exists? "/bin/sh")
                (unless (file-exists? "/bin")
                  (mkdir "/bin"))
                (symlink #$(file-append (bash-minimal) "/bin/sh") "/bin/sh"))

              ;; Setup directory for temporary files.
              (mkdir-p "/tmp")
              (for-each (lambda (var)
                          (setenv var "/tmp"))
                        '("TMPDIR" "TEMPDIR"))

              ;; Create a dummy /etc/passwd to satisfy applications that demand
              ;; to read it.
              (unless (file-exists? "/etc")
                (mkdir "/etc"))
              (write-passwd (list passwd))
              (write-group groups)

              (system* #$script)

              ;; Copy generated files to final directory.
              (for-each (lambda (output)
                          (let ((target (string-append "/gwl/" output)))
                            (mkdir-p (dirname target))
                            (copy-file output target)))
                        (filter file-exists? '#$outputs)))
            #:guest-uid uid
            #:guest-gid gid)))))

;;; ---------------------------------------------------------------------------
;;; ADDITIONAL FUNCTIONS
;;; ---------------------------------------------------------------------------

(define* (process-inputs process #:optional with-tags?)
  "Return the plain values of all inputs of PROCESS, without any
keyword tags if WITH-TAGS? is #FALSE or missing."
  (if with-tags?
      (process-raw-inputs process)
      (remove keyword? (process-raw-inputs process))))

(define* (process-outputs proc #:optional with-tags?)
  "Return the output location(s) of process PROC, without any keyword
tags if WITH-TAGS? is #FALSE or missing."
  (let* ((root (process-output-path proc))
         (mangle (if root
                     (cut string-append root "/" <>)
                     identity)))
    (if with-tags?
        (map mangle (process-outputs* proc))
        (map mangle (remove keyword? (process-outputs* proc))))))

(define (process-takes-available process)
  "Returns #T when the data inputs of the PROCESS exist."
  (match (process-inputs process)
    ((? list? inputs)
     (every file-exists? inputs))
    (_ #t)))

;;; ---------------------------------------------------------------------------
;;; DERIVATIONS AND SCRIPTS FUNCTIONS
;;; ---------------------------------------------------------------------------

;; Taken from (gnu services base)
(define* (references-file item #:optional (name "references"))
  "Return a file that contains the list of references of ITEM."
  (if (struct? item)                              ;lowerable object
      (computed-file name
                     (with-extensions (list (lookup-package "guile-gcrypt")) ;for store-copy
                       (with-imported-modules (source-module-closure
                                               '((guix build store-copy)))
                         #~(begin
                             (use-modules (guix build store-copy))

                             (call-with-output-file #$output
                               (lambda (port)
                                 (write (map store-info-item
                                             (call-with-input-file "graph"
                                               read-reference-graph))
                                        port))))))
                     #:options `(#:local-build? #false
                                 #:references-graphs (("graph" ,item))))
      (plain-file name "()")))

(define (process->script-arguments process)
  `((inputs  . ,(process-inputs process))
    (outputs . ,(process-outputs process))
    (name    . ,(process-name process))))

(define* (process->script engine #:key containerize?)
  "Build a procedure that transforms the process PROCESS into a script
and returns its location."
  (lambda* (process #:key workflow (input-files '()))
    "Return a lowerable object for the script that will execute the
process."
    (let* ((name         (process-full-name process))
           (make-wrapper (process-engine-wrapper engine))
           (packages     (cons (bash-minimal)
                               (process-packages process)))
           (manifest     (packages->manifest packages))
           (profile      (profile (content manifest)))
           (search-paths (delete-duplicates
                          (map search-path-specification->sexp
                               (manifest-search-paths manifest))))
           (out          (process-output-path process))
           (exp
            (with-imported-modules (source-module-closure (script-modules))
              #~(begin
                  #$@(if (null? packages) '()
                         `((use-modules (guix search-paths))
                           (set-search-paths (map sexp->search-path-specification
                                                  ',search-paths)
                                             (cons ,profile
                                                   ',packages))))
                  #$(if out `(setenv "out" ,out) "")
                  (setenv "_GWL_PROFILE" #$profile)
                  (use-modules (ice-9 match))
                  (match (command-line)
                    ((_ (= (lambda (s) (call-with-input-string s read))
                           #{ %gwl process-arguments}#) . rest)
                     #$(procedure->gexp process))))))
           (script
            (program-file (string-append "gwl-" name ".scm")
                          exp
                          #:guile (default-guile)))
           (computed-script
            (if containerize?
                (program-file (string-append "gwl-" name "-container.scm")
                              (containerize script
                                            #:inputs
                                            (append
                                             ;; Data inputs
                                             (process-inputs process)
                                             ;; Files mapped to free inputs
                                             input-files)
                                            #:outputs
                                            (process-outputs process))
                              #:guile (default-guile))
                script)))
      (if make-wrapper
          (make-wrapper process
                        computed-script
                        #:workflow workflow)
          computed-script))))

;;; ---------------------------------------------------------------------------
;;; CONVENIENCE FUNCTIONS
;;; ---------------------------------------------------------------------------

(define (kibibytes number)
  (* number 1024))
(define (mebibytes number)
  (* (kibibytes number) 1024))
(define (gibibytes number)
  (* (mebibytes number) 1024))

(define KiB kibibytes)
(define MiB mebibytes)
(define GiB gibibytes)

(define seconds identity)
(define (minutes number)
  (* number 60))
(define (hours number)
  (* (minutes number) 60))

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
