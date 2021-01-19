;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl packages)
  #:use-module (gwl errors)
  #:use-module ((guix packages)
                #:select (package? package-full-name))
  #:use-module ((guix inferior)
                #:select (open-inferior
                          inferior?
                          lookup-inferior-packages
                          inferior-package?
                          inferior-package-name
                          inferior-package-version))
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (current-guix

            lookup-package
            valid-package?
            package-name

            bash-minimal))

(define current-guix
  (let* ((default-guix (format #false "~a/.config/guix/current"
                               (getenv "HOME")))
         (current-guix-inferior
          (open-inferior
           (canonicalize-path default-guix))))
    (if current-guix-inferior
        (make-parameter current-guix-inferior
          (lambda (val)
            (if (inferior? val)
                val
                (raise
                 (condition
                  (&gwl-type-error
                   (expected-type "<inferior>")
                   (actual-value val)))))))
        (raise (condition
                (&formatted-message
                 (format "Could not open inferior Guix at ~a.~%")
                 (arguments (list default-guix))))))))

(define (lookup-package specification)
  (match (lookup-inferior-packages (current-guix) specification)
    ((first . rest) first)
    (_ (raise (condition
               (&gwl-package-error
                (package-spec specification)))))))

(define (valid-package? val)
  (or (package? val)
      (inferior-package? val)))

;; Just like package-full-name from (guix packages) but for inferior
;; packages.
(define* (inferior-package-full-name inferior-package #:optional (delimiter "@"))
  "Return the full name of PACKAGE--i.e., `NAME@VERSION'.  By specifying
DELIMITER (a string), you can customize what will appear between the name and
the version.  By default, DELIMITER is \"@\"."
  (string-append (inferior-package-name inferior-package)
                 delimiter
                 (inferior-package-version inferior-package)))

(define package-name
  (match-lambda
    ((? package? pkg)
     (package-full-name pkg))
    ((? inferior-package? pkg)
     (inferior-package-full-name pkg))))

(define (bash-minimal)
  (lookup-package "bash-minimal"))