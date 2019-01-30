;;; Copyright © 2016, 2018 Roel Janssen <roel@gnu.org>
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

(define-module (gwl workflows utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (success?
            successful-execution?
            source->string

            mkdir-p

            color-scheme-stepper
            %modern-color-scheme
            %light-color-scheme
            %greyscale-color-scheme))

;; Catch the return value of a call to (system* ...) and return #t when
;; it executed normally, and #f otherwise.
(define-syntax-rule (success? . body)
  (eqv? 0 (status:exit-val body)))

;; So we run multiple processes, and each process returns #t or #f, depending
;; on the successful completion of the command.  These values can be kept in
;; a list.  This function can then figure out whether all processes succeeded.
;;
(define (successful-execution? lst)
  "Returns #t when all executions succeeded, #f otherwise."
  (not (memq #f lst)))

;; This procedure returns a Scheme expression as an escaped string.
(define (source->string exp)
  (if (string? exp)
      exp
      (format #f "~s" exp)))

;; Taken from (guix build utils)
(define (mkdir-p dir)
  "Create directory DIR and all its ancestors."
  (define absolute?
    (string-prefix? "/" dir))

  (define not-slash
    (char-set-complement (char-set #\/)))

  (let loop ((components (string-tokenize dir not-slash))
             (root       (if absolute?
                             ""
                             ".")))
    (match components
      ((head tail ...)
       (let ((path (string-append root "/" head)))
         (catch 'system-error
           (lambda ()
             (mkdir path)
             (loop tail path))
           (lambda args
             (if (= EEXIST (system-error-errno args))
                 (loop tail path)
                 (apply throw args))))))
      (() #t))))

;; We like colors in diagrams.  The following closure implements an automatic
;; color stepper from which we receive a new color on each invocation.
(define (color-scheme-stepper colors)
  (let ((step -1))
    (lambda _
      (if (= (+ step 1) (length colors))
          (set! step 0)
          (set! step (+ step 1)))
      (list-ref colors step))))

;; A color scheme.
(define %modern-color-scheme
  '("#ffd42a" "#ff9955" "#d38d5f" "#ac93a7"
    "#d35f5f" "#8dd35f" "#decd87" "#bcd35f"
    "#ffe680" "#aaeeff"))

(define %light-color-scheme
  '("#ffe6d5" "#d7f4d7" "#d5f6ff" "#f4d7d7"
    "#dbdee3" "#d7e3f4" "#e3dbdb" "#fff6d5"
    "#dbe3de" "#f9f9f9" "#eeffee" "#ffd5d5"
    "#f6ffd5" "#ffeeee" "#eeeeff" "#ffd5e5"))

(define %greyscale-color-scheme
  '("#ffffff" "#eeeeee" "#dddddd" "#cccccc"
    "#bbbbbb" "#aaaaaa" "#999999" "#888888"
    "#777777" "#666666" "#555555" "#444444"))
