;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
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

(define-module (guix workflows)
  #:use-module (gwl workflows)
  #:re-export (workflow
               workflow?
               workflow-name
               workflow-full-name
               workflow-version
               workflow-input
               workflow-output
               workflow-processes
               workflow-restrictions
               workflow-arguments
               workflow-synopsis
               workflow-description

               print-workflow-record

               workflow-run-order
               workflow-prepare
               workflow-run

               ;; Syntactic sugar
               workflow:))
