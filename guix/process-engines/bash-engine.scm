;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix process-engines simple-engine)
  #:use-module (guix processes)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix profiles)
  #:use-module (guix derivations)
  #:use-module (gnu packages bash)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:export (bash-engine))

(define* (process->bash-engine-derivation proc #:key (guile (default-guile)))
  "Return an executable script that runs the PROCEDURE described in PROC, with
PROCEDURE's imported modules in its search path."
  (let* ((name               (process-full-name proc))
         (prefix             (process-engine-command-prefix simple-engine))
         (derivation-builder (process-engine-derivation-builder simple-engine))
         (simple-out         (derivation->script
                              (derivation-builder proc #:guile guile))))
    (mlet %store-monad ()
      (gexp->derivation
       name
       #~(call-with-output-file #$output
           (lambda (port)
             (use-modules (ice-9 format))
             (format port "#!~a/bin/bash~%" #$bash)
             (format port "~@[~a ~]~a~%" #$prefix #$simple-out)
             (chmod port #o555)))
       #:graft? #f))))

(define bash-engine
  (process-engine
   (name "bash-engine")
   (derivation-builder process->bash-engine-derivation)))
