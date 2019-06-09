;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix scripts workflow)
  #:use-module (gwl process-engines)
  #:use-module (gwl web-interface)
  #:use-module (gwl workflows graph)
  #:use-module (gwl workflows)
  #:use-module (gwl utils)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-1)
  #:export (guix-workflow))

(define (show-help)
  (for-each
   (lambda (line) (display line) (newline))
   '("Usage: guix workflow [OPTION]..."
     "Run multiple predefined computational process in a workflow"
     ""
     "  -i, --input=NAME=FILE  Specify workflow input NAME, optionally mapped to FILE"
     "  -o, --output=LOCATION  set LOCATION as output for a workflow"
     "  -e, --engine=ENGINE    set ENGINE, e.g. for offloading to a cluster"
     "  -p, --prepare=FILE     Prepare to run workflow from FILE"
     "  -r, --run=FILE         Run workflow from FILE"
     "  -n, --dry-run          Prepare scripts and show what would be done"
     "  -f, --force            Bypass the cache and execute all processes"
     "  -g, --graph=FILE       Load the workflow FILE and generate a graph in Dot-format"
     "  -w, --web-interface    Start the web interface"
     "  -h, --help             display this help and exit"
     "  -V, --version          display version information and exit"
     "")))

(define %options
  ;; List of command-line options.
  (list (option '(#\h "help") #f #f
                (lambda args (show-help) (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args (show-version-and-exit "guix workflow")))
        (option '(#\e "engine") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'engine arg
                              (alist-delete 'engine result))))
        (option '(#\i "input") #t #f
                (lambda (opt name arg result . rest)
                  (apply values
                         (alist-cons 'input arg result)
                         rest)))
        (option '(#\o "output") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'output arg
                              (alist-delete 'output result))))
        (option '(#\p "prepare") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'query 'prepare
                              (alist-cons 'value arg
                                          (alist-delete 'prepare result)))))
        (option '(#\r "run") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'query 'run
                              (alist-cons 'value arg
                                          (alist-delete 'run result)))))
        (option '(#\n "dry-run") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'dry-run #t result)))
        (option '(#\f "force") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'force #t result)))
        (option '(#\g "graph") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'query 'graph
                              (alist-cons 'value arg
                                          (alist-delete 'graph result)))))
        (option '(#\w "web-interface") #f #f
                (lambda args
                  (run-web-interface)
                  (exit 0)))))

(define %default-options
  `((engine . "simple-engine")))

;;;
;;; Entry point.
;;;

(define (guix-workflow . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (G_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (when (assq 'argument result)
                    (leave (G_ "~A: extraneous argument~%") arg))

                  (alist-cons 'argument arg result))
                %default-options))

  (let ((opts (parse-options)))
    (match (assoc-ref opts 'query)
      ;; Handle running or preparing workflows.
      ((and (or 'prepare 'run) action)
       (let ((wf (load-workflow (assoc-ref opts 'value)))
             (engine-name (assoc-ref opts 'engine)))
         (unless engine-name
           (leave (G_ "Please provide --engine argument.~%")))
         (let ((engine (find-engine-by-name engine-name)))
           (unless engine
             (leave (G_ "The engine ~s is not available.~%") engine-name))
           (case action
             ((prepare) (workflow-prepare wf engine))
             ((run)     (workflow-run wf engine
                                      #:inputs (filter-map (lambda (val)
                                                             (and (eq? (car val) 'input)
                                                                  (cdr val)))
                                                           opts)
                                      #:dry-run? (assoc-ref opts 'dry-run)
                                      #:force? (assoc-ref opts 'force))))))
       #t)
      ;; Handle workflow visualization
      ('graph
       (match (load-workflow (assoc-ref opts 'value))
         ((? workflow? wf)
          (format #t "~a\n" (workflow->dot wf)))
         (_ (leave (G_ "Failed to process the workflow.~%"))))
       #t)
      ;; Ignore everything else
      (_ #t))))
