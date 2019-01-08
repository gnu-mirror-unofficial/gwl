;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl process-engines grid-engine)
  #:use-module (gwl process-engines)
  #:use-module (gwl process-engines simple-engine)
  #:use-module (gwl processes)
  #:use-module (gwl workflows)
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
  (let ((bad-chars (char-set #\/ #\: #\@ #\\ #\* #\?)))
    (string-map
     (lambda (x)
       (if (char-set-contains? bad-chars x) #\- x)) name)))

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
  (if (not (process-run-time proc))
      (throw 'missing-runtime
             "Please set the run-time information for this process.")
      (let* ((name               (process-full-name proc))
             (prefix             (process-engine-command-prefix simple-engine))
             (derivation-builder (process-engine-derivation-builder simple-engine))
             (simple-out         (derivation->script
                                  (derivation-builder proc #:guile guile)))
             (time               (process-time proc))
             (space              (process-space proc))
             (threads            (process-threads proc))
             (time-str (if time
                           (format #f "-l h_rt=~a:~a:~a"
                                   (quotient time 3600) ; Hours
                                   (quotient (remainder time 3600) 60) ; Minutes
                                   (remainder time 60)) #f)) ; Seconds
             (space-str   (if space (format #f "-l h_vmem=~a"
                                            (+ space (megabytes 65))) ""))
             (threads-str (if threads (format #f "-pe threaded ~a" threads) ""))
             (logs-directory (string-append (getcwd) "/logs")))
        ;; Attempt to create the logs directory.  It's fine when it already
        ;; exists.
        (catch #t
          (lambda _ (mkdir logs-directory))
          (lambda (key . arguments) #t))
        (mlet %store-monad ()
          (gexp->derivation
           name
           #~(call-with-output-file #$output
               (lambda (port)
                 (use-modules (ice-9 format))
                 (format port "#!~a/bin/bash~%" #$bash)
                 ;; Write the SGE options to the header of the Bash script.
                 (format port
                         "#$ -S ~a/bin/bash~%~@[#$ ~a~%~]~@[#$ ~a~%~]~@[#$ ~a~]~%"
                         #$bash
                         #$space-str
                         #$time-str
                         #$threads-str)
                 ;; Write logs to the 'logs' subdirectory of the workflow output.
                 (format port "#$ -o ~a/~a.log~%#$ -e ~a/~a.errors~%~%"
                         #$logs-directory
                         #$name
                         #$logs-directory
                         #$name)
                 (format port "~@[~a ~]~a~%" #$prefix #$simple-out)
                 (chmod port #o555)))
           #:graft? #f)))))

(define grid-engine
  (process-engine
   (name "grid-engine")
   (derivation-builder process->grid-engine-derivation)
   (command-prefix "qsub")
   (restrictions-string process->grid-engine-restrictions-string)))
