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

(define-module (gwl www pages reference)
  #:use-module (gwl www pages)
  #:export (page-reference))

(define (page-reference request-path)
  (page-root-template
   "Property overview of processes and workflows" request-path
   `((h2 "Property overview of processes and workflows")

     (h3 "Processes")
     (table
      (thead
       (tr
        (th "Property")
        (th "Description")))
      (tbody
       ,(map (lambda (pair)
               (tr
                (td (car pair))
                (td (cdr pair))))
             '(("name"     . "A string to identify the process with.")
               ("version"  . "(Optional) A string to express the version of a process.")
               ("synopsis" . ""))
             ))))
   #:dependencies '(highlight)))
