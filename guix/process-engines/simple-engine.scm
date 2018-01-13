;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
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

(define-module (guix process-engines simple-engine)
  #:use-module (guix process-engines)
  #:use-module (guix processes)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix profiles)
  #:use-module (guix search-paths)
  #:use-module (guix derivations)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (simple-engine))

(define* (process->simple-engine-derivation proc #:key (guile (default-guile)))
  "Return an executable Gulie script that runs the PROCEDURE described
in PROC, with PROCEDURE's imported modules in its search path."
  (let* ((name (process-full-name proc))
         (exp (procedure->gexp proc))
         (out (process-output-path proc))
         (packages (process-package-inputs proc))
         (manifest (packages->manifest packages))
         (search-paths (map (lambda (spec)
                              (zip '(variable files separator type pattern) spec))
                            (delete-duplicates
                             (map search-path-specification->sexp
                                  (cons $PATH
                                        (append-map manifest-entry-search-paths
                                                    (manifest-entries manifest))))))))
    (mlet %store-monad ((set-load-path
                         (load-path-expression (gexp-modules exp)))
                        (profile (profile-derivation manifest)))
      (gexp->derivation
       name
       #~(call-with-output-file #$output
           (lambda (port)
             (use-modules (ice-9 pretty-print)
                          (ice-9 format))
             (format port "#!~a/bin/guile -s~%!#~%" #$guile)
             ;; The destination can be outside of the store.
             ;; TODO: We have to mount this location when building inside
             ;; a container.
             (format port "~s" #$(if out '(setenv "out" out) ""))
             (format port "~%;; Code to create a proper Guile environment.~%")
             (pretty-print '#$set-load-path port)

             ;; Load the profile that contains the programs for this
             ;; script.

             ;; TODO: for some reason we can't use match-lambda here,
             ;; which really would make this look much nicer.  Maybe
             ;; it's because it uses macros?
             (format port
                     "~%;; Prepare environment~%~{~s~}"
                     (map (lambda (spec)
                            `(setenv ,(car (assoc-ref spec 'variable))
                                     ,(string-join
                                       (map (lambda (file)
                                              (string-append #$profile "/" file))
                                            (car (assoc-ref spec 'files)))
                                       (car (assoc-ref spec 'separator)))))
                          '#$search-paths))
             (format port
                     "~%;; Set the current working directory.~%~s~%"
                     '(chdir #$(getcwd)))
             (format port "~%;; Actual code from the procedure.~%")
             (pretty-print '#$exp port)
             (chmod port #o555)))
       #:graft? #f))))

(define simple-engine
  (process-engine
   (name "simple-engine")
   (derivation-builder process->simple-engine-derivation)))
