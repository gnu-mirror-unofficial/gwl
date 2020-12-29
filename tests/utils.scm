;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (test-utils)
  #:use-module (gwl utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(test-begin "utils")

(define l '(1 2 #:mine 3 4 5 #:yours 6 7 8))

(test-equal "pick will pick the first item by default"
  3 (pick #:mine l))

(test-equal "pick can pick a single item"
  4 (pick 1 #:mine l))

(test-equal "pick does not overshoot the end of a list"
  #f (pick 10 #:mine l))

(test-equal "pick can pick a single item at the end of a sublist"
  5 (pick 2 #:mine l))

(test-equal "pick returns the list up to the next keyword"
  '(3 4 5) (pick * #:mine l))

(test-equal "pick returns the list to the end if there is no keyword"
  '(6 7 8) (pick * #:yours l))

(test-equal "pick accepts SRFI-1 accessor procedures"
  8 (pick third #:yours l))

(test-equal "expand returns list of strings"
  '("/tmp/foo/1/bar/baz"
    "/tmp/foo/2/bar/baz"
    "/home/foo/1/bar/baz"
    "/home/foo/2/bar/baz")
  (expand "/" (list "tmp" "home") "/foo/"
          (list "1" "2") "/bar/baz"))

(test-equal "expand is the list procedure when there are no options"
  '("/hello/world")
  (expand "/hello/world"))

(test-equal "expand appends parts when there are no options"
  '("/hello/world")
  (expand "/hello/" "wor" "ld"))


(test-equal "normalize-file-name: collapse slashes"
  "/hello/world"
  (normalize-file-name "/////hello////world///"))

(test-equal "normalize-file-name: remove single dots"
  "/hello/world"
  (normalize-file-name "//.///hello///./world/"))

(test-equal "normalize-file-name: resolve double dots"
  "/hello/world"
  (normalize-file-name "/bye/../hello/how/are/you/../../../world"))

(test-equal "normalize-file-name: ignore double dots beyond the root"
  "/hello/world"
  (normalize-file-name "/../../whatever/../../../bye/../hello/world"))

(test-equal "normalize-file-name: ignore double dots beyond the root"
  "/"
  (normalize-file-name "/../../"))

(test-equal "normalize-file-name: prefix relative file names 1/2"
  "./hello/world"
  (normalize-file-name "./what/../hello/world"))

(test-equal "normalize-file-name: prefix relative file names 2/2"
  "./hello/world"
  (normalize-file-name "what/../hello/world"))

(test-end "utils")
