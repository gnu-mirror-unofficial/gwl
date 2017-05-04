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

(define-module (www pages)
  #:use-module (srfi srfi-1)
  #:export (page-root-template))

(define page-title-prefix "GWL | ")

(define pages
  '(("/" "Home")
    ("/getting-started" "Getting started")
    ;("/designer" "Workflow designer")
    ("/workflow-viewer" "Workflow viewer")
    ;("/status" "System status")
    ("/help" "Help")))

(define (page-partial-main-menu request-path)
  `(ul
    ,(map
      (lambda (item)
        (if (string= (substring (car item) 1) (car (string-split (substring request-path 1) #\/)))
            `(li (@ (class "active")) ,(cadr item))
            `(li (a (@ (href ,(car item))) ,(cadr item)))))
      pages)))

(define* (page-root-template title request-path content-tree #:key (dependencies '(test)))
  `((html (@ (lang "en"))
     (head
      (title ,(string-append page-title-prefix title))
      (meta (@ (http-equiv "Content-Type") (content "text/html; charset=utf-8")))
      (link (@ (rel "icon")
               (type "image/x-icon")
               (href "/static/favicon.ico")))
      ,(if (memq 'highlight dependencies)
         `((link (@ (rel "stylesheet") (href "/static/highlight/styles/androidstudio.css")))
           (script (@ (src "/static/highlight/highlight.pack.js")) "")
           (script "hljs.initHighlightingOnLoad();"))
         `())
      ,(if (memq 'jqueryui dependencies)
         `((link (@ (rel "stylesheet") (href "/static/jqueryui/jquery-ui.theme.min.css")))
           (link (@ (rel "stylesheet") (href "/static/jqueryui/jquery-ui.css")))
           (script (@ (src "/static/jquery/jquery-3.1.1.min.js")) "")
           (script (@ (src "/static/jqueryui/jquery-ui.min.js")) "")
           (script "$(document).ready(function(){ $('.dialog').dialog({ draggable: true, modal: false, appendTo: '#workarea'}).parent().draggable({ containment: '#workarea' }) });"))
         `())
      (link (@ (rel "stylesheet")
               (href "/static/css/main.css")
               (type "text/css")
               (media "screen"))))
     (body
      (div (@ (id "wrapper"))
           (div (@ (id "header"))
                (div (@ (class "title"))
                     (h1 ,title))
                (div (@ (class "menu"))
                     ,(page-partial-main-menu request-path)))
           (div (@ (id "content"))
                ,content-tree)
           (div (@ (id "footer"))
                (p "© 2017 Roel Janssen | "
                   (a (@ (href "https://git.roelj.com/guix/gwl"))
                         "Download the source code of this page") ".")))))))
