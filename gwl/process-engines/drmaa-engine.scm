;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl process-engines drmaa-engine)
  #:use-module (gwl cache)
  #:use-module (gwl process-engines)
  #:use-module (gwl processes)
  #:use-module (gwl ui)
  #:use-module (drmaa v1 high)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (drmaa-engine))

(define* (run! #:key wrap processes)
  "Execute the scripts of all PROCESSES via DRMAA.  Wrap individual
execution with the logger/cacher procedure WRAP."
  (define* (run-drmaa #:key built-script process command cache-prefix job-name)
    (let ((jid
           (run-job
            (job-template #:job-name job-name
                          #:remote-command built-script
                          #:arguments
                          (list
                           (format #false "'~S'"
                                   (process->script-arguments process))))))
          (continuation
           (lambda ()
             ;; TODO: extract this as it's the same for all
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
                       (process-outputs process)))))
      (list jid continuation)))

  (with-drmaa-session
   (for-each (lambda (batch)
               (call-with-values
                   ;; Queue up the jobs for this batch.
                   (lambda ()
                     (unzip2
                      (filter identity
                              (match batch
                                ((? list? batch)
                                 (map (cut wrap <> run-drmaa) batch))
                                (single
                                 (list (wrap single run-drmaa)))))))
                 (lambda (jids continuations)
                   (unless (null? jids)
                     ;; Start the jobs!
                     (for-each (cut control <> 'release) jids)

                     ;; Wait for the jobs to run to completion
                     (synchronize jids))

                   ;; Run final checks
                   (for-each (lambda (continuation) (continuation))
                             continuations))))
             processes)))

(define drmaa-engine
  (process-engine
   (name "drmaa-engine")
   (run run!)))
