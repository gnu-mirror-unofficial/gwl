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

(define-module (www pages welcome)
  #:use-module (www pages)
  #:export (page-welcome))

(define (page-welcome request-path)
  (page-root-template "GWL" request-path
   `((h2 "A workflow management language empowered by GNU Guix")
     (p "Workflow management for software systems starts with getting programs "
        "to run where and when you need them.  So, naturally, we start "
        "building on top of " (a (@ (href "https://arxiv.org/abs/1305.4584"))
                                 "a language for package management") ".")

     (p "Once you can trust that running a program will yield the expected "
        "result, you need a language to describe which programs to run and "
        "with what command-line options.  Then, when you've defined the "
        "individual program invocations, you need a language to express how "
        "to combine them by describing their dependencies and relationships.")

     (p "Three domain-specific challenges implemented in one programming "
        "language, working together to make complex problems easy to describe.")

     (div (@ (style "text-align: center"))
          (div (@ (class "action-button"))
               (a (@ (href "/getting-started"))
                  "Get started →"))))))
