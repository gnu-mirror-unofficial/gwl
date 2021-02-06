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

(define-module (test-examples)
  #:use-module (gwl utils)
  #:use-module (gwl errors)
  #:use-module (srfi srfi-64))

(test-begin "examples")

(define user-module-for-file
  (@@ (gwl workflows utils) user-module-for-file))

(define wisp-reader
  (@@ (gwl workflows utils) wisp-reader))

(define read-wisp
  (lambda (port)
    (wisp-reader port (user-module-for-file "test.w"))))

(test-equal "wisp syntax produces the expected S-expression"
  '(process haiku
            (outputs "haiku.txt")
            (synopsis "Write a haiku to a file")
            (description "This process writes a haiku by Gary Hotham to the file \"haiku.txt\".")
            (procedure (quasiquote
                        (with-output-to-file (unquote outputs)
                          (lambda () (display "the library book\noverdue?\nslow falling snow"))))))

  (call-with-input-file (string-append (getenv "abs_top_srcdir")
                                       "/doc/examples/haiku.w")
    read-wisp))

(test-end "examples")
