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

(define-module (gwl www pages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (page-root-template))

(define page-title-prefix "Guix Workflow Language | ")

(define pages
  '(("/"                "Home")
    ("/getting-started" "Getting started")
    ("/beyond-started"  "Beyond started")
    ("/extended-start"  "Extended start")
    ;("/workflow-viewer" "Workflow viewer")
    ("/community"       "Community")))

(define (page-partial-main-menu request-path)
  `(ul ,(map (match-lambda
               (((? (cut string=? request-path <>)) label)
                `(li (@ (class "active")) ,label))
               ((url label)
                `(li (a (@ (href ,url)) ,label))))
             pages)))

(define* (page-root-template title request-path content-tree #:key (dependencies '()))
  `((html (@ (lang "en"))
     (head
      (title ,(string-append page-title-prefix title))
      (meta (@ (http-equiv "Content-Type") (content "text/html; charset=utf-8")))
      (link (@ (rel "icon")
               (type "image/x-icon")
               (href "/static/favicon.ico")))
      ,(if (memq 'highlight dependencies)
         `((link (@ (rel "stylesheet") (href "/static/highlight/styles/github.css")))
           (script (@ (src "/static/highlight/highlight.js")) "")
           (script (@ (type "text/javascript")) "hljs.initHighlightingOnLoad();"))
         `())
      (link (@ (rel "stylesheet")
               (href "/static/css/main.css")
               (type "text/css")
               (media "screen"))))
     (body
      (div (@ (id "wrapper"))
           (div (@ (id "header"))
                (div (@ (id "header-inner")
                        (class "width-control"))
                     (div (@ (class "logo")) (img (@ (src "/static/images/logo.png") (alt "Guix Workflow Language"))))
                     (div (@ (class "menu")) ,(page-partial-main-menu request-path))))
           (div (@ (id "content")
                   (class "width-control"))
                ,content-tree)
           (div (@ (id "footer"))
                (p (a (@ (href "https://git.sv.gnu.org/cgit/gwl.git"))
                      "Download the source code of this page") ".")))))))
