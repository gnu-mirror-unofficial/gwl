;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
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

(define-module (guix processes)
  #:use-module (guix process-engines)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9 gnu)
  #:export (process
            process?
            process-name
            process-full-name
            process-version
            process-package-inputs
            process-data-inputs
            process-complexity
            process-procedure
            process-synopsis
            process-description

            process-outputs
            process-output-path
            process-execute
            process-takes-available
            process-execute-deferred
            print-process-record

            complexity
            complexity-space
            complexity-time
            complexity-threads

            process->derivation
            process->script
            process->script->run

            ;; For the lack of a better place.
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
  (version          process-version)
  (synopsis         process-synopsis)
  (description      process-description)

  ;; Inputs can be packages, files, and settings.
  (package-inputs   process-package-inputs (default #f))
  (data-inputs      process-data-inputs (default #f))

  ;; Settings are an associated list representing (key . value) for which
  ;; command-line arguments will be available.
  ;;
  ;; So, (process (name "test") ...
  ;;              (settings '((width 200) (height 400))))
  ;;
  ;; will enable:
  ;; $ guix process --run=test --width=600 --height=1200
  ;;
  ;; Options can be discovered using:
  ;; $ guix process --list-options=test
  ;;(settings         process-settings (default #f))

  (output-path      process-output-path (default #f))
  (outputs          process-output (default #f))

  (run-time         process-complexity)
  (procedure        process-procedure))

(define (print-process process port)
  "Write a concise representation of PROCESS to PORT."
  (match process
    (($ <process> name version synopsis description run-time procedure)
     (simple-format port "#<process ~a>" (process-full-name process)))))


(define (print-process-record process port)
  "Write a multi-line representation of PROC to PORT."
  (match process
    (($ <process> name version synopsis description run-time procedure)
     (format port "name: ~a~%version: ~a~%synopsis: ~a~%description: ~a~%~%"
             name version synopsis description))))

(define (process-full-name proc)
  "Returns the name and version of PROC."
  (string-append (process-name proc) "-" (process-version proc)))

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

;;; ---------------------------------------------------------------------------
;;; ADDITIONAL FUNCTIONS
;;; ---------------------------------------------------------------------------

(define (process-with-store-monad store proc gexp-transformation callback)
  (run-with-store store
    (mlet %store-monad ((drv (gexp-transformation
                              (process-full-name proc)
                              (process-procedure proc))))
      (return (callback drv)))))

(define (process-outputs proc engine)
  "Return the output location(s) of process PROC when the derivation is built
with ENGINE."
  
  (define (compose-location folder file)
    (if file
        (string-append folder "/" file)
        folder))

  (define (get-store-location)
    (let ((derivation-builder (process-engine-derivation-builder engine)))
      (derivation->script (derivation-builder proc) #f)))

  (let ((location (process-output-path proc))
        (produces (process-output proc)))
    (if location
        (compose-location location produces)
        (compose-location (get-store-location) produces))))

(define (process-takes-available proc)
  "Returns #t when inputs exist, #f otherwise."

  (define (process-files-exist files)
    (if (null? files)
        #t
        (if (stat (car files) #f)
            (process-files-exist (cdr files))
            #f)))

  (let ((inputs (process-data-inputs proc)))
    ;; When there are no input files to take,
    ;; the situation is fine.
    (if (not inputs)
        #t
        ;; 'process-files-exist' expects a list.
        (if (list? inputs)
            (process-files-exist inputs)
            (process-files-exist (list inputs))))))

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

(define* (derivation->script drv #:optional (build? #t))
  "Write the output of a derivation DRV to a file.  When BUILD? is
set to #f, it only returns the output path."
  (let ((store (open-connection)))
    (run-with-store store
      (mlet %store-monad ((drv drv))
        (when build? (build-derivations store (list drv)))
        (return (derivation->output-path drv))))))

(define* (process->derivation proc #:key (guile (default-guile)))
  (gexp->derivation (process-full-name proc)
                    (process-procedure proc)
                    #:guile-for-build guile))

(define* (process->script proc engine #:key (stand-alone? #t))
  "Builds a derivation of PROC and displays the commands a
user needs to run."
  (if (not (process? proc))
      (format #t "This is not a process!~%")
      (let* ((command-prefix (process-engine-command-prefix engine))
             (derivation-builder (process-engine-derivation-builder engine))
             (output (derivation->script (derivation-builder proc))))
        (when stand-alone? (format #t "# Please run the following:~%~%"))
        (format #t "~@[~a ~]~a~%" command-prefix output))))

(define* (process->script->run proc engine #:key (stand-alone? #t))
  "Builds a derivation of PROC and runs the resulting script."
  (if (not (process? proc))
      (format #t "This is not a process!~%")
      (let* ((command-prefix (process-engine-command-prefix engine))
             (derivation-builder (process-engine-derivation-builder engine))
             (output (derivation->script (derivation-builder proc))))
        (system (format #f "~@[~a ~]~a~%" command-prefix output)))))
