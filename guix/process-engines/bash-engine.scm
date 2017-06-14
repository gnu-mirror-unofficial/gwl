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

(define-module (guix process-engines bash-engine)
  #:use-module (guix process-engines)
  #:use-module (guix processes)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (gnu packages bash)
  #:use-module (ice-9 pretty-print)
  #:export (bash-engine))

(define* (process->bash-engine-derivation proc #:key (guile (default-guile)))
  "Return an executable script that runs the PROCEDURE described in PROC, with
PROCEDURE's imported modules in its search path."
  (let ((name (process-full-name proc))
        (exp (process-procedure proc))
        (out (process-output-path proc)))
    (let ((out-str (if out (format #f "(setenv \"out\" ~s)" out) "")))
      (mlet %store-monad ((set-load-path
                           (load-path-expression (gexp-modules exp))))
        (gexp->derivation
         name
         (gexp
          (call-with-output-file (ungexp output)
            (lambda (port)
              (use-modules (ice-9 pretty-print))
              (format port "#!~a/bin/bash~%" (ungexp bash))
              ;; Now that we've written all of the shell code,
              ;; We can start writing the Scheme code.
              ;; We rely on Bash for this to work.
              (format port "read -d '' CODE <<EOF~%")
              ;; The destination can be outside of the store.
              ;; TODO: We have to mount this location when building inside
              ;; a container.
              (format port "~a" (ungexp out-str))
              (format port
                      "~%;; Code to create a proper Guile environment.~%~a~%"
                      (with-output-to-string
                        (lambda _ (pretty-print '(ungexp set-load-path)))))
              (format port
                      "~%;; Set the current working directory.~%(chdir ~s)~%"
                      '(ungexp (getcwd)))
              (format port "~%;; Actual code from the procedure.~%~a~%"
                      (with-output-to-string
                        (lambda _ (pretty-print '(ungexp exp)))))
              (format port "EOF~%")
              (format port "~a/bin/guile -c \"$CODE\"~%" (ungexp guile))
              (chmod port #o555))))
         #:graft? #f)))))

(define bash-engine
  (process-engine
   (name "bash-engine")
   (derivation-builder process->bash-engine-derivation)))
