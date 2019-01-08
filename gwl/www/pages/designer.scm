;;; Copyright © 2016, 2017  Roel Janssen <roel@gnu.org>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl www pages designer)
  #:use-module (gwl processes)
  #:use-module (gwl www pages)
  #:use-module (gnu processes)
  #:use-module (ice-9 vlist)
  #:export (page-designer))

(define (page-designer request-path)
  (let* ((processes (fold-processes
                     (lambda (p r)
                       (vhash-cons (process-full-name p) p r))
                     vlist-null))
         (num-processes (vlist-length processes)))
    (page-root-template "GWL" request-path
     `((h2 "Workflow management designer")
       (p "There " ,(if (> num-processes 1) "are " "is ") ,num-processes
          " available processes.")
       (div (@ (id "workarea") (style "width: 100%; height: 600pt; background: #ccc;")) ""
            ,(vlist->list
              (vlist-map
              (lambda (pair)
                `(div (@ (class "dialog")
                         (title ,(string-append (process-name (cdr pair)) " @ "
                                                (process-version (cdr pair)))))
                      (p ,(process-description (cdr pair)))))
              processes))))
     #:dependencies '(jqueryui))))
