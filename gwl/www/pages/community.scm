;;; Copyright © 2016, 2017  Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (gwl www pages community)
  #:use-module (gwl www pages)
  #:export (page-community))

(define (page-community request-path)
  (page-root-template "Guix Workflow Language" request-path
   `((h2 "Community")
     (p "Please interact with us through the "
        (a (@ (href "https://lists.gnu.org/mailman/listinfo/gwl-devel"))
           "mailing list") "."))))
