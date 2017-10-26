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

(define-module (gwl www pages welcome)
  #:use-module (gwl www pages)
  #:export (page-welcome))

(define (page-welcome request-path)
  (page-root-template "GWL" request-path
   `((h2 "A workflow management language extension for GNU Guix")

     (p "This workflow language provides an extension to GNU Guix's "
        (a (@ (href "https://arxiv.org/abs/1305.4584"))
           "declarative language for package management") " to automate "
           "execution of programs.  Additionally, GWL can use Grid Engine to "
           "offload program execution.")
     
     (div (@ (style "text-align: center"))
          (div (@ (class "action-button"))
               (a (@ (href "/getting-started"))
                  "Get started →"))))))
