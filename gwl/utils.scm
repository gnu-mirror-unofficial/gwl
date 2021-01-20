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
  #:use-module (gwl workflows)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-31)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (system base language)
  #:use-module (language wisp)
  #:export (load-workflow
            on
            pick
            file
            files

            wisp-suffix
            normalize-file-name))

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

(define (wisp-suffix file)
  (cond ((string-suffix? ".w" file) ".w")
        ((string-suffix? ".wisp" file) ".wisp")
        ((string-suffix? ".gwl" file) ".gwl")
        (else #f)))

;; Taken from (guix ui).
(define (make-user-module modules)
  "Return a new user module with the additional MODULES loaded."
  ;; Module in which the machine description file is loaded.
  (let ((module (make-fresh-user-module)))
    (for-each (lambda (iface)
                (module-use! module (resolve-interface iface)))
              modules)
    module))

(define (load-workflow* file)
  "Load the workflow specified in FILE in the context of a new module
where all the basic GWL modules are available."
  (define modules
    (if (wisp-suffix file)
        '((gwl processes)
          (gwl workflows)
          (gwl sugar)
          (gwl utils)
          (srfi srfi-1)
          (srfi srfi-26)
          (srfi srfi-88))
        '((gwl processes)
          (gwl workflows)
          (gwl sugar)
          (gwl utils)
          (srfi srfi-1)
          (srfi srfi-26))))
  (let ((result (load* file (make-user-module modules))))
    (unless (workflow? result)
      (raise (condition
              (&gwl-error)
              (&formatted-message
               (format "File `~a' does not evaluate to a workflow value.~%")
               (arguments (list file))))))
    result))

;; Helper to handle relative file names.
(define-syntax-rule (load-workflow file)
  (let ((target (string-append (dirname (or (current-filename)
                                            (*current-filename*) ""))
                               "/" file)))
    (load-workflow*
     (if (or (absolute-file-name? file)
             (not (file-exists? target)))
         file target))))


(define wisp-reader
  ;; XXX We'd like to use language-reader here, but (language wisp
  ;; spec) triggers a very annoying setlocale warning because it
  ;; evaluates (setlocale LC_ALL "foo").
  #;
  (language-reader (lookup-language 'wisp))
  (lambda (port env)
    ;; allow using "# foo" as #(foo).
    (read-hash-extend #\# (λ (chr port) #\#))
    (cond
     ((eof-object? (peek-char port))
      (read-char port)) ; return eof: we’re done
     (else
      (match (wisp-scheme-read-chunk port)
        (() #false)
        ((chunk . _) chunk))))))

;; Adapted from (guix ui).
(define* (load* file user-module)
  "Load the user provided Scheme or Wisp source code FILE."
  (define tag
    (make-prompt-tag "user-code"))

  (catch #t
    (lambda ()

      ;; Force re-compilation to avoid ABI issues
      (set! %fresh-auto-compile #t)
      (set! %load-should-auto-compile #t)

      (save-module-excursion
       (lambda ()
         (set-current-module user-module)

         ;; Hide the "auto-compiling" messages.
         (parameterize ((current-warning-port (%make-void-port "w")))
           (call-with-prompt tag
             (lambda ()
               ;; XXX: The Wisp reader fails to set source properties in all
               ;; cases, so (current-filename) always returns #F.
               (module-define! user-module '*current-filename*
                               (make-parameter file))
               ;; Give 'load' an absolute file name so that it doesn't
               ;; try to search for FILE in %LOAD-COMPILED-PATH.
               (load (canonicalize-path file)
                     (and (wisp-suffix file)
                          (lambda (port)
                            (wisp-reader port user-module)))))
             (const #f))))))
    (lambda _
      (exit 1))
    (rec (handle-error . args)
         ;; Capture the stack up to this procedure call, excluded, and pass
         ;; the faulty stack frame to 'report-load-error'.
         (let* ((stack (make-stack #t handle-error tag))
                (frame (last-frame-with-source stack
                                               (basename (canonicalize-path file)))))
           (report-load-error file args frame)))))

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
