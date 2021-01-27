;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2012-2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gwl utils)
  #:use-module (gwl ui)
  #:use-module (gwl errors)
  #:use-module (gwl processes)
  #:use-module ((gwl workflows utils)
                #:select (load-workflow))
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (require-packages
            on
            pick
            get
            file
            files

            normalize-file-name)
  #:re-export (load-workflow))

;; Convenience procedure to simplify Wisp syntax of higher-order
;; procedures such as "map" by having the collection first.
(define (on collection higher-proc item-proc)
  (higher-proc item-proc collection))

;; Simplify access to tagged items in lists.
(define pick
  (case-lambda
    ;; First item.
    ((key collection)
     (when (null? collection)
       (raise (condition
               (&gwl-error)
               (&message
                (message "pick: Cannot pick from empty collection.~%")))))
     (or (and=> (memq key collection) cadr)
         (raise (condition
                 (&gwl-error)
                 (&formatted-message
                  (format "pick: Cannot find item with key `~a'.~%")
                  (arguments (list key)))))))
    ;; Nth item
    ((n key collection)
     (when (null? collection)
       (raise (condition
               (&gwl-error)
               (&message
                (message "pick: Cannot pick from empty collection.~%")))))
     (let ((sub
            (and=> (memq key collection)
                   (lambda (sublist)
                     (break keyword? (cdr sublist))))))
       (unless sub
         (raise (condition
                 (&gwl-error)
                 (&formatted-message
                  (format "pick: Cannot pick item with key `~a'.~%")
                  (arguments (list key))))))
       (cond
        ((number? n)
         (let ((len (length sub)))
           (unless (> len n)
             (raise (condition
                     (&gwl-error)
                     (&formatted-message
                      (format "pick: Cannot pick item number ~a, there are only ~a items.~%")
                      (arguments (list n len)))))))
         (list-ref sub n))
        ;; All items
        ((and (procedure? n)
              (eq? (procedure-name n) '*))
         sub)
        ;; SRFI-1 accessors like "first"
        ((procedure? n)
         (or (n sub)
             (raise (condition
                     (&gwl-error)
                     (&formatted-message
                      (format "pick: Could not pick item with selector `~a'~%")
                      (arguments (list (procedure-name n))))))))
        (else
         (raise (condition
                 (&gwl-type-error
                  (expected-type (list "<number>" "<procedure>"))
                  (actual-value n))))))))))

;; TODO: perhaps use guile-lens instead?
(define get
  (let ((no-default (gensym)))
    (lambda* (collection #:key (default no-default) #:rest path)
      "Look up each key from PATH, a list of strings or symbols, in the
nested COLLECTION, an association list.  Raise a &gwl-error if any of
the elements of PATH could not be found in the nested collection."
      (define path*
        (match path
          ((#:default value . rest) rest)
          (_ path)))
      (define (recurse node breadcrumbs remaining-path)
        (catch 'escape-get
          (lambda ()
            (match remaining-path
              (() node)
              ((next . rest)
               (unless (pair? node)
                 (throw 'escape-get
                        (condition
                         (&gwl-error)
                         (&formatted-message
                          (format "Cannot look up element ~a in something that is not a collection:~%  ~y~%")
                          (arguments (list
                                      (if (null? breadcrumbs)
                                          next
                                          (format #false "`~a' (following ~{~a~^ -> ~})" next (reverse breadcrumbs)))
                                      node))))))
               (let ((target (assoc-ref node next)))
                 (unless target
                   (throw 'escape-get
                          (condition
                           (&gwl-error)
                           (&formatted-message
                            (format "No element ~a in collection:~%  ~y~%")
                            (arguments (list
                                        (if (null? breadcrumbs)
                                            next
                                            (format #false "`~a' (following ~{~a~^ -> ~})" next (reverse breadcrumbs)))
                                        node))))))
                 (recurse target (cons next breadcrumbs) rest)))))
          (lambda (key condition)
            (if (eq? default no-default)
                (raise condition)
                default))))
      (recurse collection '() path*))))

(define (expand . file-parts)
  "Expand the file name template consisting of strings interspersed
with lists of strings to a list of concrete file names of all
combinations."
  (define (inner prefix parts)
    (match parts
      (((? string? part) . rest)
       (inner (string-append prefix part) rest))
      (((? list? ls) . rest)
       (append-map (lambda (part)
                     (inner (string-append prefix part) rest))
                   ls))
      (_ (list prefix))))
  (inner "" file-parts))

(define (slash-transformer parts)
  (let loop ((parts parts)
             (result (list)))
    (if (null? parts)
        (reverse result)
        (syntax-case (car parts) (/)
          (/
           (loop (cdr parts)
                 (cons #'"/" result)))
          (s
           (loop (cdr parts)
                 (cons #'s result)))))))

(define-syntax file
  (lambda (x)
    (syntax-case x ()
      ((_ rest ...)
       #`(normalize-file-name
          (string-append
           #,@(slash-transformer #'(rest ...))))))))

(define-syntax files
  (lambda (x)
    (syntax-case x ()
      ((_ rest ...)
       #`(map normalize-file-name
              (expand
               #,@(slash-transformer #'(rest ...))))))))


;; This is a marker at the top of a workflow file.  It is a list of
;; package names that will be made available before the workflow is
;; evaluated.
(define require-packages list)

(define (normalize-file-name file-name)
  "Return FILE-NAME after collapsing slashes, removing \".\" directory
components, and resolving \"..\"."
  (let* ((components
          (string-tokenize file-name (char-set-complement (char-set #\/))))
         (without-dots
          (reverse
           (fold (lambda (component result)
                   (if (string=? ".." component)
                       (match result
                         ((previous . rest) rest)
                         (_ '()))
                       (cons component result)))
                 '()
                 (delete "."
                         components))))
         (tail (string-join without-dots "/")))
    (string-append (if (absolute-file-name? file-name)
                       ""
                       ".")
                   "/" tail)))
