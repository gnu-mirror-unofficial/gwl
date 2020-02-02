;;; Copyright © 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; This module is deprecated.

(define-module (guix processes)
  #:use-module (gwl processes)
  #:re-export (process
               process?
               process-name
               process-full-name
               process-version
               process-packages
               process-inputs
               process-run-time
               process-procedure
               process-synopsis
               process-description

               process-outputs
               process-output-path
               process-takes-available
               print-process-record

               complexity
               complexity-space
               complexity-time
               complexity-threads

               process->script
               process->script->run

               ;; Convenience functions
               minutes
               hours

               process-space
               process-time
               process-threads

               processes-filter
               processes-filter-by-name

               code-snippet
               code-snippet?
               code-snippet-language

               procedure->gexp)
  #:export (process-package-inputs
            process-data-inputs
            gigabytes
            megabytes
            kilobytes
            derivation->script
            default-guile))

(define process-package-inputs process-packages)
(define process-data-inputs process-inputs)
(define kilobytes kibibytes)
(define megabytes mebibytes)
(define gigabytes gibibytes)

(define (default-guile)
  (module-ref (resolve-interface '(gnu packages commencement))
              'guile-final))

(define derivation->script identity)
