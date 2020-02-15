;;; Copyright © 2017, 2018 Roel Janssen <roel@gnu.org>
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

(define-module (gwl process-engines)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (process-engine
            process-engine?
            process-engine-name
            process-engine-wrapper
            process-engine-runner
            find-engine-by-name))

;;; ---------------------------------------------------------------------------
;;; RECORD TYPES
;;; ---------------------------------------------------------------------------

;;
;; A process-engine determines how a process is executed.
;;
(define-class <process-engine> ()
  (name
   #:init-keyword #:name
   #:accessor process-engine-name)
  (wrapper
   #:init-value #f
   #:init-keyword #:wrapper
   #:accessor process-engine-wrapper)
  (runner
   #:init-value '("/bin/sh" "-c")
   #:init-keyword #:runner
   #:accessor process-engine-runner))

;; Convenient DSL-like constructor.
(define-syntax process-engine
  (lambda (x)
    (syntax-case x ()
      ((_ fields ...)
       #`(let* (#,@(map (lambda (field)
                          (syntax-case field ()
                            ((empty-field)
                             (syntax-violation #f "process-engine: Empty field" #'empty-field))
                            ((field single-value)
                             #'(field single-value))))
                        #'(fields ...)))
           (make <process-engine>
             #,@(append-map (lambda (field)
                              (syntax-case field ()
                                ((name . rest)
                                 #`((symbol->keyword 'name) name))))
                            #'(fields ...))))))))

(define (process-engine? thing)
  (is-a? thing <process-engine>))

(define (print-process-engine engine port)
  "Write a concise representation of PROCESS-ENGINE to PORT."
  (simple-format port "#<process-engine ~a>"
                 (process-engine-name engine)))

(define-method (write (engine <process-engine>) port)
  (print-process-engine engine port))

(define (find-engine-by-name name)
  "Find the process engine corresponding to NAME."
  (let ((engine-symbol (string->symbol name)))
    (false-if-exception (module-ref
                         (resolve-interface
                          `(gwl process-engines ,engine-symbol))
                         engine-symbol))))
