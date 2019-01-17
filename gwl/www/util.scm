;;; Copyright © 2016  Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl www util)
  #:use-module (srfi srfi-1)
  #:use-module (web request)
  #:use-module (web uri)
  #:export (directory?
            file-extension
            string-replace-occurrence
            request-path-components))

(define (directory? filename)
  (string=? filename (dirname filename)))

(define (file-extension file-name)
  (last (string-split file-name #\.)))

(define (string-replace-occurrence str occurrence alternative)
  (string-map (lambda (x) (if (eq? x occurrence) alternative x)) str))

(define request-path-components
  (compose split-and-decode-uri-path uri-path request-uri))
