;;; Copyright © 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gwl errors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-64))

(test-begin "utils")

(define l '(1 2 #:mine 3 4 5 #:yours 6 7 8))

(test-equal "pick will pick the first item by default"
  3 (pick #:mine l))

(test-equal "pick can pick a single item"
  4 (pick 1 #:mine l))

(test-error "pick does not overshoot the end of a list"
            &gwl-error
            (pick 10 #:mine l))

(test-equal "pick can pick a single item at the end of a sublist"
  5 (pick 2 #:mine l))

(test-equal "pick returns the list up to the next keyword"
  '(3 4 5) (pick * #:mine l))

(test-equal "pick returns the list to the end if there is no keyword"
  '(6 7 8) (pick * #:yours l))

(test-equal "pick accepts SRFI-1 accessor procedures"
  8 (pick third #:yours l))

(test-assert "pick complains if the collection is empty 1/3"
  (string-contains
   (guard (c ((gwl-error? c)
              (condition-message c)))
     (pick 1 #:yours '()))
   "Cannot pick from empty collection"))

(test-assert "pick complains if the collection is empty 2/3"
  (string-contains
   (guard (c ((gwl-error? c)
              (condition-message c)))
     (pick third #:yours '()))
   "Cannot pick from empty collection"))

(test-assert "pick complains if the collection is empty 3/3"
  (string-contains
   (guard (c ((gwl-error? c)
              (condition-message c)))
     (pick * #:yours '()))
   "Cannot pick from empty collection"))

(test-equal "pick complains if the selector type is bogus"
  '("<number>" "<procedure>")
  (guard (c ((gwl-type-error? c)
             (condition-ref c 'expected-type)))
    (pick "foo" #:mine l)))


;; expand is used internally by "files"
(define expand (@@ (gwl utils) expand))

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


(test-equal "file: returns a simple string"
  "/hello/world"
  (file / "hello" / "world"))

(test-equal "file: returns a simple string with simple variable"
  "/hello/world"
  (let ((who "world"))
    (file / "hello" / who)))

(test-equal "file: normalizes file name"
  "/hello/world"
  (let ((who "world"))
    (file / "hello" / / / who / / ".." / who /)))

(test-equal "files: returns a list"
  '("/hello/world" "/bye/world")
  (files / (list "hello" "bye") / "world"))


(test-equal "get: returns an element by string key in a simple alist"
  "world"
  (let ((collection
         `(("hello" . "world")
           ("bye"   . "nobody"))))
    (get collection "hello")))

(test-equal "get: returns an element by symbol key in a simple alist"
  "nobody"
  (let ((collection
         `((hello . "world")
           (bye   . "nobody"))))
    (get collection 'bye)))

(test-equal "get: returns an element by path in a nested alist"
  "hi"
  (let ((collection
         `((greeting . ((world . "hello")
                        (friend . "hi")))
           (bye      . "nobody"))))
    (get collection 'greeting 'friend)))

(test-assert "get: complains when a path leads nowhere"
  (condition-has-type?
   (guard (c ((gwl-error? c) c))
     (let ((collection
            `((greeting . ((world . "hello")
                           (friend . "hi")))
              (bye      . "nobody"))))
       (get collection 'greeting 'foe)))
   &gwl-error))

(test-assert "get: complains when there is no collection 1/2"
  (condition-has-type?
   (guard (c ((gwl-error? c) c))
     (let ((collection
            `((greeting . ((world . "hello")
                           (friend . "hi")))
              (bye      . "nobody"))))
       (get collection 'greeting 'world 'later)))
   &gwl-error))

(test-assert "get: complains when there is no collection 2/2"
  (condition-has-type?
   (guard (c ((gwl-error? c) c))
     (let ((collection 102))
       (get collection 'greeting 'world 'later)))
   &gwl-error))

(test-equal "get: returns provided default value on not found error"
  "cheers"
  (let ((collection
         `((greeting . ((world . "hello")
                        (friend . "hi")))
           (bye      . "nobody"))))
    (get collection #:default "cheers" 'greeting 'world 'later)))

(test-equal "get: returns provided default value when there is no collection"
  "cheers"
  (let ((collection 102))
    (get collection #:default "cheers" 'greeting 'world 'later)))

(test-equal "get: returns expected value even when provided default value"
  "hello"
  (let ((collection
         `((greeting . ((world . "hello")
                        (friend . "hi")))
           (bye      . "nobody"))))
    (get collection #:default "cheers" 'greeting 'world)))

(test-end "utils")
