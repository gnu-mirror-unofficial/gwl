;;; Copyright © 2016, 2017, 2018  Roel Janssen <roel@gnu.org>
;;; Copyright © 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl web-interface)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module ((rnrs bytevectors) #:select (utf8->string))
  #:use-module (ice-9 match)
  #:use-module (gwl config)
  #:use-module (gwl www render)
  #:use-module (gwl www util)
  #:use-module (gwl www pages)
  #:use-module (gwl www pages error)
  #:use-module (gwl www pages welcome)
  #:export (run-web-interface))

(define* (render-scheme-page request-path path #:optional data)
  (or (and-let* ((module (resolve-module `(gwl www pages ,@path) #:ensure #f))
                 (page-symbol (symbol-append 'page- (last path)))
                 (page-proc (false-if-exception
                             (module-ref module page-symbol))))
        (render-html (if data
                         (page-proc request-path #:post-data data)
                         (page-proc request-path))))
      (not-found (page-error-404 request-path))))

(define (request-handler request request-body)
  (apply values
         (match (cons (request-method request)
                      (request-path-components request))
           (('GET "static" path ...)
            (render-static-file path))
           (('GET)
            (render-html (page-welcome "/")))
           (('GET path ...)
            (render-scheme-page
             (uri-path (request-uri request))
             (map string->symbol path)))
           (('POST path ...)
            (render-scheme-page
             (uri-path (request-uri request))
             (map string->symbol path)
             (utf8->string request-body)))
           (oops
            (not-found (page-error-404 oops))))))

(define %default-socket-options
  ;; List of options passed to 'setsockopt' when transmitting files.
  (list (list SO_SNDBUF (* 208 1024))))

(define* (configure-socket socket #:key (level SOL_SOCKET)
                           (options %default-socket-options))
  "Apply multiple option tuples in OPTIONS to SOCKET, using LEVEL."
  (for-each (cut apply setsockopt socket level <>)
            options))

(define (open-server-socket address)
  "Return a TCP socket bound to ADDRESS, a socket address."
  (let ((sock (socket (sockaddr:fam address) SOCK_STREAM 0)))
    (configure-socket sock #:options (cons (list SO_REUSEADDR 1)
                                           %default-socket-options))
    (bind sock address)
    sock))

(define* (run-web-interface #:optional
                            (host (%config 'host))
                            (port (%config 'port)))
  (let* ((address (let ((addr host))
                    (make-socket-address (sockaddr:fam addr)
                                         (sockaddr:addr addr)
                                         port)))
         (socket  (open-server-socket address)))
    (format (current-error-port)
            "GWL web service is running at http://~a:~a~%"
            (inet-ntop (sockaddr:fam address) (sockaddr:addr address))
            (sockaddr:port address))
    (format (current-error-port)
            "Press C-c C-c to quit.~%")
    (run-server request-handler 'http
                `(#:socket ,socket))))
