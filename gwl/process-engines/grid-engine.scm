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
  #:use-module ((guix monads) #:select (mbegin))
  #:use-module ((guix store) #:select (%store-monad))
  #:use-module (gnu packages bash)
  #:use-module (ice-9 format)
  #:export (grid-engine))

(define sanitize-sge-job-name
  (let ((bad-chars (char-set #\/ #\: #\@ #\\ #\* #\?)))
    (lambda (name)
      (string-map
       (lambda (x)
         (if (char-set-contains? bad-chars x) #\- x)) name))))

(define (process-job-name proc)
  "Returns a valid job name for PROC."
  (string-append "gwl-"
                 (sanitize-sge-job-name
                  (process-full-name proc))))

(define (process->grid-engine-restrictions-string process workflow)
  "Return a grid engine option string to specify the process name for
PROCESS alongside all jobs that it depends on according to WORKFLOW."
  (let ((restrictions
         (and (workflow? workflow)
              (or (assoc-ref (workflow-restrictions workflow) process)
                  '()))))
    (string-join (cons* "-N" (process-job-name process)
                        (map (lambda (job)
                               (format #f "-hold_jid ~a"
                                       (process-job-name job)))
                             restrictions)))))

(define (process->grid-engine-time-limit process)
  "Return a grid engine limit string corresponding to the time
requirements of PROCESS."
  (or (and=> (process-time process)
             (lambda (time)
               (format #f "-l h_rt=~a:~a:~a"
                       (quotient time 3600)                ; Hours
                       (quotient (remainder time 3600) 60) ; Minutes
                       (remainder time 60))))              ; Seconds
      ""))

(define (process->grid-engine-space-limit process)
  "Return a grid engine limit string corresponding to the memory
requirements of PROCESS."
  (or (and=> (process-space process)
             (lambda (space)
               (format #f "-l h_vmem=~a"
                       (+ space (megabytes 65)))))
      ""))

(define* (process->grid-engine-derivation process
                                          #:key
                                          workflow
                                          (guile (default-guile)))
  "Return an executable script that runs the PROCEDURE described in
the PROCESS, with the procedure's imported modules in its load path."
  (let* ((name               (process-full-name process))
         (derivation-builder (process-engine-derivation-builder simple-engine))
         (simple-out         (derivation->script
                              (derivation-builder process #:guile guile)))
         (restrictions       (process->grid-engine-restrictions-string process workflow))
         (time-str           (process->grid-engine-time-limit process))
         (space-str          (process->grid-engine-space-limit process))
         (threads-str        (or (and=> (process-threads process)
                                        (lambda (threads)
                                          (format #f "-pe threaded ~a" threads)))
                                 ""))
         (logs-directory (string-append (getcwd) "/logs")))
    (unless (file-exists? logs-directory)
      (mkdir logs-directory))
    (mbegin %store-monad
      (gexp->derivation
       name
       #~(call-with-output-file #$output
           (lambda (port)
             (use-modules (ice-9 format))
             (format port "#!~a/bin/bash~%" #$bash)
             ;; Write the SGE options to the header of the Bash script.
             (format port
                     "#$ -S ~a/bin/bash~%~@[#$ ~a~%~]~@[#$ ~a~%~]~@[#$ ~a~%~]~@[#$ ~a~]~%"
                     #$bash
                     #$restrictions
                     #$space-str
                     #$time-str
                     #$threads-str)
             ;; Write logs to the 'logs' subdirectory of the workflow output.
             (format port "#$ -o ~a/~a.log~%#$ -e ~a/~a.errors~%~%"
                     #$logs-directory
                     #$name
                     #$logs-directory
                     #$name)
             (format port "~a~%" #$simple-out)
             (chmod port #o555)))
       #:graft? #f))))

(define grid-engine
  (process-engine
   (name "grid-engine")
   (derivation-builder process->grid-engine-derivation)
   (command-prefix "qsub")))
