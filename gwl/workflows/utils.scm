;;; Copyright © 2016, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl workflows utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-31)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (language wisp)
  #:use-module (system base language)
  #:use-module ((system base compile)
                #:select (default-optimization-level))

  #:use-module (gwl config)
  #:use-module (gwl errors)
  #:use-module (gwl packages)
  #:use-module (gwl workflows)
  #:use-module (gwl ui)

  #:use-module (guix profiles)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix search-paths)
  #:use-module ((guix derivations)
                #:select (built-derivations
                          derivation-outputs
                          derivation-output-path))
  #:use-module ((guix ui)
                #:select (build-notifier))
  #:use-module ((guix status)
                #:select (with-status-verbosity))

  #:export (success?
            successful-execution?
            source->string

            mkdir-p

            color-scheme-stepper
            %modern-color-scheme
            %light-color-scheme
            %greyscale-color-scheme

            wisp-suffix
            load-workflow))

;; Catch the return value of a call to (system* ...) and return #t when
;; it executed normally, and #f otherwise.
(define-syntax-rule (success? . body)
  (eqv? 0 (status:exit-val body)))

;; So we run multiple processes, and each process returns #t or #f, depending
;; on the successful completion of the command.  These values can be kept in
;; a list.  This function can then figure out whether all processes succeeded.
;;
(define (successful-execution? lst)
  "Returns #t when all executions succeeded, #f otherwise."
  (not (memq #f lst)))

;; This procedure returns a Scheme expression as an escaped string.
(define (source->string exp)
  (if (string? exp)
      exp
      (format #f "~s" exp)))

;; Taken from (guix build utils)
(define (mkdir-p dir)
  "Create directory DIR and all its ancestors."
  (define absolute?
    (string-prefix? "/" dir))

  (define not-slash
    (char-set-complement (char-set #\/)))

  (let loop ((components (string-tokenize dir not-slash))
             (root       (if absolute?
                             ""
                             ".")))
    (match components
      ((head tail ...)
       (let ((path (string-append root "/" head)))
         (catch 'system-error
           (lambda ()
             (mkdir path)
             (loop tail path))
           (lambda args
             (if (= EEXIST (system-error-errno args))
                 (loop tail path)
                 (apply throw args))))))
      (() #t))))

;; We like colors in diagrams.  The following closure implements an automatic
;; color stepper from which we receive a new color on each invocation.
(define (color-scheme-stepper colors)
  (let ((step -1))
    (lambda _
      (if (= (+ step 1) (length colors))
          (set! step 0)
          (set! step (+ step 1)))
      (list-ref colors step))))

;; A color scheme.
(define %modern-color-scheme
  '("#ffd42a" "#ff9955" "#d38d5f" "#ac93a7"
    "#d35f5f" "#8dd35f" "#decd87" "#bcd35f"
    "#ffe680" "#aaeeff"))

(define %light-color-scheme
  '("#ffe6d5" "#d7f4d7" "#d5f6ff" "#f4d7d7"
    "#dbdee3" "#d7e3f4" "#e3dbdb" "#fff6d5"
    "#dbe3de" "#f9f9f9" "#eeffee" "#ffd5d5"
    "#f6ffd5" "#ffeeee" "#eeeeff" "#ffd5e5"))

(define %greyscale-color-scheme
  '("#ffffff" "#eeeeee" "#dddddd" "#cccccc"
    "#bbbbbb" "#aaaaaa" "#999999" "#888888"
    "#777777" "#666666" "#555555" "#444444"))


(define (wisp-suffix file)
  (cond ((string-suffix? ".w" file) ".w")
        ((string-suffix? ".wisp" file) ".wisp")
        ((string-suffix? ".gwl" file) ".gwl")
        (else #f)))

(define wisp (lookup-language 'wisp))
(define wisp-reader (language-reader wisp))


;;; Workflow profile and environment

(define (required-packages file)
  "Read the optional package declaration at the beginning of the
workflow specified in FILE.  Return a list of package names or the
empty list."
  (define read*
    (if (wisp-suffix file)
        (lambda (port)
          (wisp-reader port (user-module-for-file file)))
        read))
  (define declaration?
    (call-with-input-file file
      (lambda (port)
        (let loop ((result (read* port)))
          (if (eof-object? result) #false
              (or result (loop (read* port))))))))
  (match declaration?
    (('require-packages packages ...)
     packages)
    (_ '())))

(define (activate-workflow-environment! file-name)
  "Set the environment variables specified by MANIFEST for the
PROFILE-DIRECTORY.  Augment existing environment variables with
additional search paths.  Handle Guile's load paths separately to
modify the load path of the current process."
  (define new-load-path '())
  (define new-load-compiled-path '())
  (and-let*
      ((package-names (required-packages file-name))
       (_assert       (not (null? package-names)))
       (manifest      (packages->manifest
                       (map lookup-package package-names)))
       (profile       (profile (content manifest)))
       (profile-directory
        (with-status-verbosity (%config 'verbosity)
          (with-build-handler (build-notifier #:verbosity (%config 'verbosity))
            (run-with-store (inferior-store)
              (mlet* %store-monad
                  ((drv (lower-object profile))
                   (_ (built-derivations (list drv))))
                (match (derivation-outputs drv)
                  (((_ . output) . rest)
                   (return (derivation-output-path output))))))))))
    (for-each (match-lambda
                ((($ <search-path-specification>
                     "GUILE_LOAD_PATH" _ separator) . value)
                 (set! new-load-path
                       (append (parse-path value)
                               new-load-path)))
                ((($ <search-path-specification>
                     "GUILE_LOAD_COMPILED_PATH" _ separator) . value)
                 (set! new-load-compiled-path
                       (append (parse-path value)
                               new-load-compiled-path)))
                ((($ <search-path-specification> variable _ separator) . value)
                 (let ((current (getenv variable)))
                   (setenv variable
                           (if current
                               (if separator
                                   (string-append value separator current)
                                   value)
                               value)))))
              (profile-search-paths profile-directory manifest))
    (unless (null? new-load-path)
      (set! %load-path
            (delete-duplicates
             (append new-load-path %load-path))))
    (unless (null? new-load-compiled-path)
      (set! %load-compiled-path
            (delete-duplicates
             (append new-load-compiled-path %load-compiled-path))))))

;; Taken from (guix ui).
(define (make-user-module modules)
  "Return a new user module with the additional MODULES loaded."
  ;; Module in which the machine description file is loaded.
  (let ((module (make-fresh-user-module)))
    (for-each (lambda (iface)
                (module-use! module (resolve-interface iface)))
              modules)
    module))

(define (user-module-for-file file)
  (define modules
    (if (wisp-suffix file)
        '((gwl processes)
          (gwl workflows)
          (gwl sugar)
          (gwl utils)
          (srfi srfi-1)
          (srfi srfi-26)
          (srfi srfi-88))
        '((gwl processes)
          (gwl workflows)
          (gwl sugar)
          (gwl utils)
          (srfi srfi-1)
          (srfi srfi-26))))
  (make-user-module modules))

(define (load-workflow* file)
  "Load the workflow specified in FILE in the context of a new module
where all the basic GWL modules are available."
  (activate-workflow-environment! file)
  (let ((result (load* file (user-module-for-file file))))
    (unless (workflow? result)
      (raise (condition
              (&gwl-error)
              (&formatted-message
               (format "File `~a' does not evaluate to a workflow value.~%")
               (arguments (list file))))))
    result))

;; Helper to handle relative file names.
(define-syntax-rule (load-workflow file)
  (let ((target (string-append (dirname (or (current-filename)
                                            (*current-filename*) ""))
                               "/" file)))
    (load-workflow*
     (if (or (absolute-file-name? file)
             (not (file-exists? target)))
         file target))))

;; Adapted from (guix ui).
(define* (load* file user-module)
  "Load the user provided Scheme or Wisp source code FILE."
  (define tag
    (make-prompt-tag "user-code"))

  (catch #t
    (lambda ()
      (log-event 'info (G_ "Loading workflow file `~a'...~%") file)

      ;; Force re-compilation to avoid ABI issues
      (set! %fresh-auto-compile #t)
      (set! %load-should-auto-compile #t)

      (save-module-excursion
       (lambda ()
         (set-current-module user-module)

         (parameterize ((default-optimization-level 0) ; make compilation fast
                        (current-language (if (wisp-suffix file)
                                              wisp (current-language)))
                        ;; Hide the "auto-compiling" messages.
                        (current-warning-port (%make-void-port "w")))
           (call-with-prompt tag
             (lambda ()
               ;; XXX: The Wisp reader fails to set source properties in all
               ;; cases, so (current-filename) always returns #F.
               (module-define! user-module '*current-filename*
                               (make-parameter file))
               ;; Give 'load' an absolute file name so that it doesn't
               ;; try to search for FILE in %LOAD-COMPILED-PATH.
               (load (canonicalize-path file)))
             (const #f))))))
    (lambda _
      (exit 1))
    (rec (handle-error . args)
         ;; Capture the stack up to this procedure call, excluded, and pass
         ;; the faulty stack frame to 'report-load-error'.
         (let* ((stack (make-stack #t handle-error tag))
                (frame (last-frame-with-source stack
                                               (basename (canonicalize-path file)))))
           (report-load-error file args frame)))))
