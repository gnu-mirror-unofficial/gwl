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

(define-module (guix process-engines grid-engine)
  #:use-module (guix process-engines)
  #:use-module (guix processes)
  #:use-module (guix workflows)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix profiles)
  #:use-module (guix derivations)
  #:use-module (gnu packages bash)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (grid-engine))

(define (sanitize-sge-job-name name)
  (string-map
   (lambda (x) (if (or (eq? x #\/) (eq? x #\:) (eq? x #\@)
                       (eq? x #\\) (eq? x #\*) (eq? x #\?)) #\- x)) name))

(define (process-job-name proc)
  "Returns a valid job name for PROC."
  (string-append "gwl-"
                 (sanitize-sge-job-name
                  (process-full-name proc))))

(define (process->grid-engine-restrictions-string proc workflow)
  (let ((restrictions (if (workflow? workflow)
                          (assoc-ref (workflow-restrictions workflow) proc)
                          #f)))
    (string-append "-N " (process-job-name proc) " "
      (if restrictions
          (format #f "~{-hold_jid ~a ~}"
                  (map (lambda (proc)
                         (process-job-name proc))
                       restrictions))
          ""))))

(define* (process->grid-engine-derivation proc #:key (guile (default-guile)))
  "Return an executable script that runs the PROCEDURE described in PROC, with
PROCEDURE's imported modules in its search path."
  (let* ((name (process-full-name proc))
         (exp (process-procedure proc))
         ;(out (process-output-path proc))
         (packages (process-package-inputs proc))
         (time (if (process-complexity proc)
                   (complexity-time (process-complexity proc)) 0))
         (space (if (process-complexity proc)
                    (complexity-space (process-complexity proc)) 0))
         (threads (complexity-threads (process-complexity proc)))
         (time-str (if time
                       (format #f "-l h_rt=~a:~a:~a"
                               (quotient time 3600) ; Hours
                               (quotient (remainder time 3600) 60) ; Minutes
                               (remainder time 60)) #f)) ; Seconds
         (space-str   (if space (format #f "-l h_vmem=~a"
                                        (+ space (megabytes 65))) ""))
         (threads-str (if threads (format #f "-pe threaded ~a" threads) ""))
         (logs-directory (string-append (getcwd) "/logs")))
         ;(out-str (if out (format #f "(setenv \"out\" ~s)" out) ""))
    (mlet %store-monad ((set-load-path
                         (load-path-expression (gexp-modules exp)))
                        (profile (profile-derivation
                                    (packages->manifest packages))))
      (gexp->derivation
       name
       (gexp
        (call-with-output-file (ungexp output)
          (lambda (port)
            (use-modules (ice-9 pretty-print)
                         (ice-9 format))
            (format port "#!~a/bin/bash~%" (ungexp bash))
            ;; Write the SGE options to the header of the Bash script.
            (format port
                    "#$ -S ~a/bin/bash~%~@[#$ ~a~%~]~@[#$ ~a~%~]~@[#$ ~a~%~]~%"
                    (ungexp bash)
                    (ungexp space-str)
                    (ungexp time-str)
                    (ungexp threads-str))
            ;; Write logs to the 'logs' subdirectory of the workflow output.
            (format port "#$ -o ~a/~a.log~%#$ -e ~a/~a.errors~%"
                    (ungexp logs-directory)
                    (ungexp name)
                    (ungexp logs-directory)
                    (ungexp name))
            ;; Load the profile that contains the programs for this script.
            (format port "source ~a/etc/profile~%" (ungexp profile))
            ;; Now that we've written all of the SGE shell code,
            ;; We can start writing the Scheme code.
            ;; We rely on Bash for this to work.
            (format port "read -d '' CODE <<EOF~%")
            ;; The destination can be outside of the store.
            ;; Note: We have to mount this location when building inside
            ;; a container.
            ;;(format port "~a" (ungexp out-str))
            ;; Change to the correct output directory.
            ;; We use the pretty-printer so that users can debug their
            ;; procedures more easily.
            (format port
                    "~%;; Code to create a proper Guile environment.~%~a~%"
                    (with-output-to-string
                      (lambda _ (pretty-print '(ungexp set-load-path)))))
            (format port
                    "~%;; Set the current working directory.~%(chdir ~s)~%"
                    '(ungexp (getcwd)))
            (format port "~%;; Create the 'logs' directory.~%")
            (format port "(catch #t (lambda _ (mkdir ~s))~%"
                    (ungexp logs-directory))
            (format port "          (lambda (key . parameters) #t))~%")
            (format port "~%;; Actual code from the procedure.~%~a~%"
                    (with-output-to-string
                      (lambda _ (pretty-print '(ungexp exp)))))
            ;;(format port ";; Write a 'completed' file.~%")
            ;;(format port "(call-with-output-file ~s (const #t))~%"
            ;;        (string-append (ungexp out) "/JOB_DONE"))
            (format port "EOF~%")
            (format port "~a/bin/guile -c \"$CODE\"~%" (ungexp guile))
            (chmod port #o555))))
       #:graft? #f))))

(define grid-engine
  (process-engine
   (name "grid-engine")
   (derivation-builder process->grid-engine-derivation)
   (command-prefix "qsub")
   (restrictions-string process->grid-engine-restrictions-string)))
