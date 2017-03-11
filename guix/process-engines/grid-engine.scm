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
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (gnu packages bash)
  #:use-module (ice-9 pretty-print)
  #:export (grid-engine))

(define* (process->grid-engine-derivation proc #:key (guile (default-guile)))
  "Return an executable script that runs the PROCEDURE described in PROC, with
PROCEDURE's imported modules in its search path."
  (let* ((name (process-full-name proc))
         (exp (process-procedure proc))
         ;(out (process-output-path proc))
         (time (complexity-time (process-complexity proc)))
         (space (complexity-space (process-complexity proc)))
         (threads (complexity-threads (process-complexity proc)))
         (time-str (if time
                       (format #f "-l h_rt=~a:~a:~a"
                               (quotient time 3600) ; Hours
                               (quotient (remainder time 3600) 60) ; Minutes
                               (remainder time 60)) #f)) ; Seconds
         (space-str   (if space (format #f "-l h_vmem=~a" space) ""))
         (threads-str (if threads (format #f "-pe threaded ~a" threads) "")))
         ;(out-str (if out (format #f "(setenv \"out\" ~s)" out) ""))
    (mlet %store-monad ((set-load-path
                         (load-path-expression (gexp-modules exp))))
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
            (format port ";; Actual code from the procedure.~%~a~%"
                    (with-output-to-string
                      (lambda _ (pretty-print '(ungexp exp)))))
            ;;(format port ";; Write a 'completed' file.~%")
            ;;(format port "(call-with-output-file ~s (const #t))~%"
            ;;        (string-append (ungexp out) "/JOB_DONE"))
            (format port "EOF~%")
            (format port "~a/bin/guile -c \"$CODE\"~%" (ungexp guile))
            (chmod port #o555))))))))

(define grid-engine
  (process-engine
   (name "grid-engine")
   (derivation-builder process->grid-engine-derivation)
   (command-prefix "qsub")))
