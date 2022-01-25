;;; Copyright © 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (test-cache)
  #:use-module (gwl processes)
  #:use-module (gwl workflows)
  #:use-module (gwl process-engines simple-engine)
  #:use-module (gwl cache)
  #:use-module ((guix base32)
                #:select (bytevector->base32-string))
  #:use-module ((gcrypt hash)
                #:select (sha256))
  #:use-module ((rnrs bytevectors)
                #:select (u8-list->bytevector))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(test-begin "cache")

(define input-file
  (let* ((port (mkstemp! "/tmp/gwl-test-input華-XXXXXX"))
         (name (port-filename port)))
    (display "this is a test input" port)
    (close port)
    name))

(define p1 (make-process (name "p1") (procedure '())))
(define p2 (make-process (name "p2") (procedure '())))
(define p3 (make-process (name "p3") (procedure '())))
(define p4 (make-process (name "p4") (inputs input-file) (procedure '())))
(define p5 (make-process (name "p5") (procedure '())))
(define p6 (make-process (name "p6") (procedure '())))

(define wf
  (make-workflow
   (name "test-workflow")
   (processes
    (graph (p2 -> p1)
           (p3 -> p4)
           (p5 -> p2 p3 p4)
           (p6 -> p5)))))

(define %cache-prefix
  (format #false "~a/foo/bar/baz/"
          (or (getenv "TMPDIR") "/tmp")))

(define computed-workflow
  ((@@ (gwl workflows) make-computed-workflow) wf
   (const "name")
   (const #false)
   (workflow-run-order wf #:parallel? #true)
   (const %cache-prefix)))

;; Flat list of processes
(define ordered-processes
  (append-map (match-lambda
                ((? list? l) l)
                (l (list l)))
              (computed-workflow-ordered-processes computed-workflow)))

(define scripts-table
  (make-hash-table))

(define workflow->data-hashes
  (@@ (gwl cache) workflow->data-hashes))

(test-assert "workflow->data-hashes returns a list"
  (list? (workflow->data-hashes wf
                                ordered-processes
                                '()
                                scripts-table)))

(test-assert "workflow->data-hashes returns an alist where all processes are keys"
  (let ((hashes (workflow->data-hashes wf
                                       ordered-processes
                                       '()
                                       scripts-table)))
    (every (lambda (process)
             (assoc-ref hashes process))
           ordered-processes)))

(test-assert "workflow->data-hashes returns an alist where all values are strings"
  (let ((hashes (workflow->data-hashes wf
                                       ordered-processes
                                       '()
                                       scripts-table)))
    (every string? (map cdr hashes))))


(define process->hash (@@ (gwl cache) process->hash))
(define hash-input-file (@@ (gwl cache) hash-input-file))

(define (hashes->hash-string hashes)
  (bytevector->base32-string
   (sha256
    (u8-list->bytevector
     (apply append hashes)))))

(test-equal "workflow->data-hashes hashes just the script for an independent process"
  (hashes->hash-string
   (list (process->hash p1 scripts-table)))
  (assoc-ref (workflow->data-hashes wf
                                    ordered-processes
                                    '()
                                    scripts-table)
             p1))

(test-equal "workflow->data-hashes hashes the script and its inputs"
  (hashes->hash-string
   (list (process->hash p4 scripts-table)
         (hash-input-file input-file)))
  (assoc-ref (workflow->data-hashes wf
                                    ordered-processes
                                    (list
                                     (list input-file input-file))
                                    scripts-table)
             p4))

(test-equal "workflow->data-hashes hashes all dependencies of a process"
  (hashes->hash-string
   (list (process->hash p3 scripts-table)
         (process->hash p4 scripts-table)
         (hash-input-file input-file)))
  (assoc-ref (workflow->data-hashes wf
                                    ordered-processes
                                    (list
                                     (list input-file input-file))
                                    scripts-table)
             p3))

(test-assert "cache! creates directories as needed"
  (begin
    (cache! input-file %cache-prefix)
    (file-exists? (string-append %cache-prefix input-file))))

(test-end "cache")
