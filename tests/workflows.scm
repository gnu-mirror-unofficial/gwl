;;; Copyright © 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (test-workflows)
  #:use-module (gwl processes)
  #:use-module (gwl workflows)
  #:use-module (gwl workflows utils)
  #:use-module (gwl workflows execution-order)
  #:use-module (language wisp spec)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

(test-begin "workflows")

;; 1 -> 2 -\
;;   -> 3 -> 5 --> 6
;; 4 ------/

(define p1 (make-process (name "p1") (procedure '())))
(define p2 (make-process (name "p2") (procedure '())))
(define p3 (make-process (name "p3") (procedure '())))
(define p4 (make-process (name "p4") (procedure '())))
(define p5 (make-process (name "p5") (procedure '())))
(define p6 (make-process (name "p6") (procedure '())))

(define wf
  (make-workflow
   (name "test-workflow")
   (processes
    (list p1 p2 p3 p4 p5 p6))
   (restrictions
    (list (list p2 p1)
          (list p3 p1)
          (list p5 p2 p3 p4)
          (list p6 p5)))))

(define wf2
  (make-workflow
   (name "test-workflow")
   (processes
    (graph (p2 -> p1)
           (p3 -> p1)
           (p5 -> p2 p3 p4)
           (p6 -> p5)))))

(test-equal "deprecated restrictions can be expressed with processes field"
  (workflow-restrictions wf2)
  (workflow-restrictions wf))

(test-assert "sequential-execution-order"
  (let* ((order (sequential-execution-order
                 (workflow-processes wf)
                 (workflow-restrictions wf)))
         (first-then (lambda (a b)
                       (call-with-values
                           (lambda ()
                             (partition (cut eq? a <>) order))
                         (lambda (before after)
                           (find (cut eq? b <>) after))))))
    (and (first-then p1 p2)
         (first-then p1 p3)
         (first-then p1 p5)
         (first-then p1 p6)
         (first-then p4 p5)
         (first-then p4 p6)
         (first-then p2 p5)
         (first-then p2 p6)
         (first-then p3 p5)
         (first-then p3 p6))))

(test-assert "parallel-step-execution-order"
  (every (lambda (a b) (lset= eq? a b))
         (parallel-step-execution-order
          (workflow-processes wf)
          (workflow-restrictions wf))
         (list (list p1 p4)
               (list p2 p3)
               (list p5)
               (list p6))))

(test-equal "wisp syntax produces the expected S-expression"
  (with-input-from-string "\
make-workflow
  name \"test-workflow\"
  processes
    graph
      p2 -> p1
      p3 -> p1
      p5 -> p2 p3 p4
      p6 -> p5
"
    (lambda ()
      ((@@ (language wisp spec) read-one-wisp-sexp)
       (current-input-port)
       (current-module))))
  '(make-workflow
    (name "test-workflow")
    (processes
     (graph (p2 -> p1)
            (p3 -> p1)
            (p5 -> p2 p3 p4)
            (p6 -> p5)))))

(test-assert "make-workflow permits definitions in field values"
  (let* ((eat (lambda (what)
                (make-process
                 (name (string-append "eat-" what))
                 (procedure '()))))
         (wf (make-workflow
                (name "test-workflow")
                (processes
                 (define eat-corn (eat "corn"))
                 (define eat-salad (eat "salad"))
                 (graph
                  (p1 -> eat-corn eat-salad)
                  (eat-salad -> p2))))))
    (workflow? wf)))

(define inputs->map (@@ (gwl workflows) inputs->map))
(test-equal "inputs->map returns the empty list for an empty list of inputs"
  '()
  (inputs->map '()))
(test-equal "inputs->map returns a normalized list for plain input file names"
  '(("this" "this")
    ("that" "that"))
  (inputs->map '("this" "that")))
(test-equal "inputs->map returns a normalized list for mapped input file names"
  '(("this" "THIS")
    ("that" "THAT"))
  (inputs->map '("this=THIS" "that=THAT")))
(test-equal "inputs->map returns a normalized list for all file names"
  '(("this" "THIS")
    ("that" "that")
    ("anything" "anything"))
  (inputs->map '("this=THIS" "that" "anything")))

(define (wf3 prefix)
  (define p1
    (make-process
     (name "p1")
     (inputs (map (cut string-append prefix <>)
                  (list "first" "second")))
     (outputs (string-append prefix "third"))
     (procedure '())))
  (define p2
    (make-process
     (name "p2")
     (inputs (map (cut string-append prefix <>)
                  (list "third" "fourth")))
     (outputs (string-append prefix "fifth"))
     (procedure '())))
  (define p3
    (make-process
     (name "p3")
     (inputs (list (string-append prefix "fifth")))
     (procedure '())))
  (make-workflow
   (name "test-workflow")
   (processes
    (auto-connect p1 p2 p3))))

(define (make-prefix)
  (let ((prefix (string-append (tmpnam) "/")))
    (mkdir-p prefix)
    prefix))

(let ((prefix (make-prefix)))
  (test-equal "workflow-free-inputs returns all free inputs"
    (map (cut string-append prefix <>)
         (list "first" "second" "fourth"))
    (workflow-free-inputs (wf3 prefix))))

(define input-file
  (let* ((port (mkstemp! "/tmp/gwl-test-input-XXXXXX"))
         (name (port-filename port)))
    (display "this is a test input" port)
    (close port)
    name))
(define (inputs-map-full prefix)
  (list
   (list (string-append prefix "first") input-file)
   (list (string-append prefix "second") input-file)
   (list (string-append prefix "fourth") input-file)))


(define prepare-inputs (@@ (gwl workflows) prepare-inputs))
(let* ((prefix (make-prefix))
       (inputs-map (inputs-map-full prefix)))
  (test-equal "prepare-inputs returns the unmodified inputs-map when all files exist"
    inputs-map
    (prepare-inputs (wf3 prefix) inputs-map)))

(let ((prefix (make-prefix)))
  (test-error "prepare-inputs throws a missing-inputs condition"
              (@@ (gwl workflows) &missing-inputs)
              (prepare-inputs (wf3 prefix) '()))
  (test-equal "prepare-inputs throws a condition listing the missing input files"
    (map (cut string-append prefix <>)
         (list "first" "second" "fourth"))
    (guard (condition
            (((@@ (gwl workflows) missing-inputs-condition?) condition)
             ((@@ (gwl workflows) missing-inputs-files) condition)))
      (prepare-inputs (wf3 prefix) '()))))

(let ((prefix (make-prefix)))
  ;; Create all undeclared files
  (for-each (lambda (name)
              (with-output-to-file (string-append prefix name)
                (lambda () (display name))))
            (list "first" "second" "fourth"))
  (test-equal "prepare-inputs adds found files to an empty inputs-map"
    (map (lambda (name)
           (let ((file-name (string-append prefix name)))
             (list file-name file-name)))
         (list "first" "second" "fourth"))
    (prepare-inputs (wf3 prefix) '())))

(let* ((prefix (make-prefix))
       (incomplete-inputs-map
        (list
         (list (string-append prefix "first") input-file))))
  ;; Create undeclared files
  (for-each (lambda (name)
              (with-output-to-file (string-append prefix name)
                (lambda () (display name))))
            (list "second" "fourth"))
  (test-equal "prepare-inputs adds found files to the incomplete inputs-map"
    (append incomplete-inputs-map
            (map (lambda (name)
                   (let ((file-name (string-append prefix name)))
                     (list file-name file-name)))
                 (list "second" "fourth")))
    (prepare-inputs (wf3 prefix)
                    incomplete-inputs-map)))

(test-end "workflows")
