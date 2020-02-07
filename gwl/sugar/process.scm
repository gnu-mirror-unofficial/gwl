;;; Copyright © 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl sugar process)
  #:use-module ((gwl processes) #:select (make-process))
  #:export (process))

;; Shorter syntax, which is especially useful when wisp is used.
(define-syntax process
  (lambda (x)
    (syntax-case x (with)
      ;; Parameterized process
      ((_ id (with . args) rest ...)
       (if (assoc-ref (syntax->datum #'(rest ...)) 'name)
           #`(define-public id
               (lambda* args
                 (make-process rest ...)))
           (with-syntax ((the-name (datum->syntax x 'name)))
             #`(define-public id
                 (let ((the-name #,(symbol->string (syntax->datum #'id))))
                   (lambda* args
                     (make-process
                      (name the-name)
                      rest ...)))))))
      ((_ id rest ...)
       (with-syntax ((the-name (datum->syntax x 'name)))
         #`(define-public id
             (let ((the-name #,(symbol->string (syntax->datum #'id))))
               (make-process
                (name the-name)
                rest ...))))))))
