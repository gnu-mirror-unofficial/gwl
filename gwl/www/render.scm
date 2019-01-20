;;; Copyright © 2016, 2017, 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl www render)
  #:use-module (gwl www config)
  #:use-module (gwl www util)
  #:use-module (gwl www pages error)
  #:use-module (sxml simple)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 binary-ports)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (render-static-file
            render-html
            not-found
            redirect))

(define file-mime-types
  '(("css"  . (text/css))
    ("js"   . (text/javascript))
    ("json" . (text/javascript))
    ("html" . (text/html))
    ("png"  . (image/png))
    ("svg"  . (image/svg+xml))
    ("ico"  . (image/x-icon))
    ("pdf"  . (application/pdf))
    ("woff" . (application/font-woff))
    ("ttf"  . (application/font-sfnt))))

(define (render-html sxml)
  (list '((content-type . (text/html)))
        (cut sxml->xml sxml <>)))

(define (not-found uri-or-sxml)
  (list (build-response #:code 404
                        #:headers '((content-type . (text/html))))
        (if (uri? uri-or-sxml)
            (string-append "<html><body>Resource not found: "
                           (uri->string uri-or-sxml)
                           "</body></html>")
            (cut sxml->xml uri-or-sxml <>))))

(define (render-static-file path)
  ;; PATH is a list of path components
  (let ((file-name (string-join (cons* (web-config 'static-root) path) "/")))
    (if (or (any (cut string-contains <> "..") path)
            (not (file-exists? file-name))
            (directory? file-name))
        (not-found (build-uri 'http
                              #:port (web-config 'port)
                              #:path (string-join path "/" 'prefix)))
        ;; Check file size before returning the file.
        (let ((file-stat (stat file-name)))
          (if (> (stat:size file-stat) (web-config 'max-file-size))
              (render-html (page-error-filesize path))
              (list `((content-type . ,(or (assoc-ref file-mime-types
                                                      (file-extension file-name))
                                           '(text/plain))))
                    (call-with-input-file file-name get-bytevector-all)))))))
