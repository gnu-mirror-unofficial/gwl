;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
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

(define-module (gwl process-engines simple-engine)
  #:use-module (gwl process-engines)
  #:use-module (gwl processes)
  #:use-module (guix gexp)
  #:use-module ((guix derivations)
                #:select (build-derivations
                          derivation->output-path))
  #:use-module ((guix store)
                #:select (%store-monad with-store run-with-store))
  #:use-module ((guix monads)
                #:select (mlet))
  #:use-module ((guix profiles)
                #:select (packages->manifest
                          profile-derivation
                          manifest-entries
                          manifest-entry-search-paths))
  #:use-module ((guix packages)
                #:select (package-output package?))
  #:use-module ((guix search-paths)
                #:select (search-path-specification->sexp
                          $PATH))
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (simple-engine))

(define* (process->simple-engine-derivation process
                                            #:key
                                            (guile (default-guile))
                                            workflow)
  "Return a derivation for an executable Guile script that runs the
procedure described in PROCESS, with the procedure's imported modules in
its search path."

  (let* ((name (process-full-name process))
         (exp (procedure->gexp process))
         (out (process-output-path process))
         (packages (process-packages process))
         (manifest (packages->manifest packages))
         (search-paths (delete-duplicates
                        (map search-path-specification->sexp
                             (cons $PATH
                                   (append-map manifest-entry-search-paths
                                               (manifest-entries manifest)))))))
    (with-store store
      (run-with-store store
        (mlet %store-monad ((set-load-path
                             (load-path-expression (gexp-modules exp)))
                            (profile (profile-derivation manifest)))
          ;; Ensure that the manifest is in fact instantiated.
          (build-derivations store (list profile))
          (let ((profile-dir (derivation->output-path profile)))
            (gexp->derivation
             name
             #~(begin
                 (use-modules (ice-9 pretty-print)
                              (ice-9 format)
                              (ice-9 match)
                              (srfi srfi-26))
                 (call-with-output-file #$output
                   (lambda (port)
                     (format port "#!~a/bin/guile --no-auto-compile~%-s~%!#~%" #$guile)
                     (pretty-print
                      '#$(containerize
                          `(begin
                             ;; The destination can be outside of the store.
                             ;; TODO: We have to mount this location when building inside
                             ;; a container.
                             ,(if out `(setenv "out" ,out) "")
                             (setenv "_GWL_PROFILE" ,profile-dir)
                             ";; Code to create a proper Guile environment."
                             ,set-load-path
                             ,@(map (match-lambda
                                      ((variable files separator type pattern)
                                       `(setenv ,variable
                                                (string-join
                                                 (map (lambda (file)
                                                        (string-append ,profile-dir "/" file))
                                                      ',files)
                                                 ,separator))))
                                    search-paths)
                             ,exp)
                          #:inputs
                          (cons profile-dir
                                (append
                                 ;; Individual package locations
                                 (map (lambda (pkg) (package-output store pkg)) packages)
                                 ;; Data inputs
                                 (remove keyword? (process-inputs process))))
                          #:outputs
                          (process-outputs process))
                      port)
                     (chmod port #o555)))))))))))

(define simple-engine
  (process-engine
   (name "simple-engine")
   (derivation-builder process->simple-engine-derivation)
   (runner '("/bin/sh" "-c"))))
