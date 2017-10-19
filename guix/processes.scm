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

            ;; Convenience functions
            gigabytes
            megabytes
            kilobytes
            minutes
            hours

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
  (version          process-version        (default ""))
  (synopsis         process-synopsis       (default ""))
  (description      process-description    (default ""))

  ;; Inputs can be packages, files, and settings.
  (package-inputs   process-package-inputs (default '()))
  (data-inputs      process-data-inputs    (default #f))

  ;; Outputs can be anything, but are mostly files (I guess).
  (output-path      process-output-path    (default #f))
  (outputs          process-output         (default #f))

  (run-time         process-complexity     (default #f))
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

;;; ---------------------------------------------------------------------------
;;; ADDITIONAL FUNCTIONS
;;; ---------------------------------------------------------------------------

(define (process-with-store-monad store proc gexp-transformation callback)
  (run-with-store store
    (mlet %store-monad ((drv (gexp-transformation
                              (process-full-name proc)
                              (process-procedure proc))))
      (return (callback drv)))))

(define (process-outputs proc)
  "Return the output location(s) of process PROC."
  
  (define (compose-location folder file)
    (cond
     ((and file folder)  (string-append folder "/" file))
     (file file)
     (else folder)))

  (let ((path (process-output-path proc))
        (file (process-output proc)))
    (compose-location path file)))

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
                    (process-procedure proc)
                    #:guile-for-build guile
                    #:graft? #f))

(define* (process->script proc engine #:key (stand-alone? #t)
                                            (workflow '()))
  "Builds a derivation of PROC and displays the commands a
user needs to run."
  (if (not (process? proc))
      (format #t "This is not a process!~%")
      (catch #t
        (lambda _
          (let* ((command-prefix (process-engine-command-prefix engine))
                 (derivation-builder (process-engine-derivation-builder engine))
                 (output (derivation->script (derivation-builder proc)))
                 (restrictions-func (process-engine-restrictions-string engine))
                 (restrictions (restrictions-func proc workflow)))
            (when stand-alone? (format #t "# Please run the following:~%~%"))
            (format #t "~@[~a ~]~@[~a ~]~a~%" command-prefix restrictions output)))
        (lambda (key . args)
          (match key
            ('missing-runtime
             (format #t "Error in process '~a':~%  ~a~%"
                     (process-full-name proc) (car args)))
            ('match-error
             (let ((inputs (process-package-inputs proc)))
               (format #t "Error in process '~a':~%  ~a~%"
                       (process-full-name proc)
                       (if (and (not (null? inputs))
                                (not (process? (car inputs))))
                           "Please unquote the value of 'package-inputs'."
                           (car args)))))
            (_
             (format #t "Unknown error in process '~a':~%  ~s.~%"
                     (process-full-name proc) (car args))))))))

(define* (process->script->run proc engine #:key (stand-alone? #t)
                                                 (workflow '()))
  "Builds a derivation of PROC and runs the resulting script."
  (if (not (process? proc))
      (format #t "This is not a process!~%")
      (system (with-output-to-string
                (lambda _
                  (process->script proc engine
                                   #:workflow workflow
                                   #:stand-alone? #f))))))

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
