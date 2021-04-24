;;; Copyright © 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl ui)
  #:use-module (gwl config)
  #:use-module (gwl errors)
  #:use-module (guix colors)
  #:use-module (srfi srfi-26)
  #:export (G_
            log-event

            print-diagnostic-prefix

            %error-color
            %hint-color
            %info-color
            %debug-color
            %execute-color))

;; TODO: add gettext support
(define (G_ msg) msg)

(define %hint-color (color BOLD CYAN))
(define %error-color (color BOLD RED))
(define %info-color (color BOLD BLUE))
(define %debug-color (color BOLD MAGENTA))
(define %execute-color (color BOLD YELLOW))

(define* (print-diagnostic-prefix prefix #:optional location
                                  #:key (colors (color)))
  "Print PREFIX as a diagnostic line prefix."
  (define color?
    (color-output? (current-error-port)))

  (define location-color
    (if color?
        (cut colorize-string <> (color BOLD))
        identity))

  (define prefix-color
    (if color?
        (lambda (prefix)
          (colorize-string prefix colors))
        identity))

  (if (location? location)
      (format (current-error-port) "~a: ~a"
              (location-color (location->string location))
              (prefix-color prefix))
      (format (current-error-port) "~a"
              (prefix-color prefix))))

(define (log-event type . message)
  (define print?
    (or (member 'all (%config 'log-events))
        (member type (%config 'log-events))))
  (when print?
    (case type
      ((error)
       (print-diagnostic-prefix (G_ "error: ") #:colors %error-color))
      ((info)
       (print-diagnostic-prefix (G_ "info: ") #:colors %info-color))
      ((execute)
       (print-diagnostic-prefix (G_ "run: ") #:colors %execute-color))
      ((cache)
       (print-diagnostic-prefix (G_ "cache: ") #:colors %debug-color))
      ((debug)
       (print-diagnostic-prefix (G_ "debug: ") #:colors %debug-color))
      ((process)
       (print-diagnostic-prefix (G_ "process: ") #:colors %execute-color))
      ((guix)
       (print-diagnostic-prefix (G_ "guix: ") #:colors %execute-color))
      (else #true))
    (force-output (current-error-port))
    (apply format (current-error-port) message)))
