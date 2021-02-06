;;; Copyright © 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl oop)
  #:use-module (gwl errors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (oop goops)
  #:export (<gwl-class>))

(define-class <gwl-class> (<object>))

;; As much as I'd like it, we cannot deal with implicit lists here
;; because lists may contain keyword tagged items.  Keywords in lists
;; may very well clash with class initialization keywords, so we need
;; to use a macro to handle implicit lists.  This initialization
;; method will only ever receive initargs with proper keyword/value
;; pairs.

;; TODO: maybe move validation to setters instead of only running them
;; at initialization time.
(define-method (initialize (instance <gwl-class>) initargs)
  (define klass (class-of instance))
  ;; Validate slots
  (for-each
   (lambda (slot)
     (let* ((options (slot-definition-options slot))
            (keyword (slot-definition-init-keyword slot)))
       ;; Ensure required slots are filled.
       (and (memq #:required? options)
            (or (member keyword initargs)
                (error (format #f
                               "~a: Required field `~a' missing.~%"
                               (class-name klass)
                               (slot-definition-name slot)))))

       ;; Only perform these checks if a value is provided.
       (and=> (member keyword initargs)
              (lambda (tail)
                ;; Ensure that values for fields accepting implicit lists are
                ;; normalized.
                (and (memq #:implicit-list? options)
                     (match tail
                       ((_ (? (negate list?) value) . rest)
                        (list-cdr-set! (memq keyword initargs)
                                       0 (cons (list value) rest)))
                       (_ #t))) ; it's a list, let it be

                (and (memq #:implicit-concatenation? options)
                     (match tail
                       ((_ (? (negate string?) value) . rest)
                        (list-cdr-set! (memq keyword initargs)
                                       0 (cons (apply string-append value) rest)))
                       (_ #t)))
                ;; Run transformers on slot values
                (match (memq #:transformer options)
                  ((_ transform . rest)
                   (match tail
                     ((_ value . rest)
                      (let ((new-value (transform instance value)))
                        (list-cdr-set! (memq keyword initargs)
                                       0 (cons new-value rest))))))
                  (_ #t))

                ;; Run validators on slot values
                (match (memq #:validator options)
                  ((_ validate . rest)
                   (match tail
                     ((_ value . rest)
                      ;; TODO: allow for better error messages
                      (or (validate value)
                          (error (format #f
                                         "~a: field `~a' has the wrong type.~%"
                                         (class-name klass)
                                         (slot-definition-name slot)))))))
                  (_ #t))))))
   (class-slots klass))

  ;; Reject extraneous fields
  (let* ((allowed (map slot-definition-init-keyword (class-slots klass)))
         (provided (filter keyword? initargs)))
    (match (lset-difference eq? provided allowed)
      (() #t)
      (extraneous
       (raise (condition
               (&gwl-error)
               (&formatted-message
                (format "~a: extraneous fields: ~{~a ~}~%")
                (arguments (list (class-name klass)
                                 (map keyword->symbol extraneous)))))))))
  (next-method))
