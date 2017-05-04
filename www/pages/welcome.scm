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

     (p "This workflow language provides a lightweight framework to write "
        "pipelines, and integrates with your computing cluster.  You can "
        "develop in any programming language, and combine the resulting "
        "scripts and programs with a "
        (a (@ (href "https://arxiv.org/abs/1305.4584"))
           "declarative language for package management") ", and the GWL.")

     (div (@ (style "text-align: center;"))
          (img (@ (src "/static/images/what-is-gwl.svg")
                  (style "height: 175pt"))))

     (p "So, with the GWL you are able to write pipelines that can be "
        "reproduced on computers and computing clusters without dealing "
        "with the details on those other clusters.")

     (div (@ (style "text-align: center"))
          (div (@ (class "action-button"))
               (a (@ (href "/getting-started"))
                  "Get started →"))))))
