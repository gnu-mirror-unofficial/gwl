;;; Copyright © 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9 gnu)
  #:export (process-engine
            process-engine?
            process-engine-name
            process-engine-derivation-builder
            process-engine-command-prefix
            process-engine-restrictions-string
            find-engine-by-name))

;;; ---------------------------------------------------------------------------
;;; RECORD TYPES
;;; ---------------------------------------------------------------------------

;;
;; A process-engine determines which 
;;
(define-record-type* <process-engine>
  process-engine make-process-engine
  process-engine?

  (name                 process-engine-name)
  (derivation-builder   process-engine-derivation-builder)
  (command-prefix       process-engine-command-prefix (default #f))
  (restrictions-string  process-engine-restrictions-string
                        (default (const #f))))

(define (print-process-engine engine port)
  "Write a concise representation of PROCESS-ENGINE to PORT."
  (match engine
    (($ <process-engine> name derivation-builder)
     (simple-format port "#<process-engine ~a>" (process-engine-name engine)))))

(set-record-type-printer! <process-engine> print-process-engine)

(define (find-engine-by-name name)
  "Find the process engine corresponding to NAME."
  (let* ((engine-symbol (string->symbol name)))
    (false-if-exception (module-ref
                         (resolve-interface
                          `(gwl process-engines ,engine-symbol))
                         engine-symbol))))
