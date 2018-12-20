;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
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

(define-module (guix scripts workflow)
  #:use-module (gwl process-engines)
  #:use-module (gwl web-interface)
  #:use-module (gwl workflows graph)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix utils)
  #:use-module (guix workflows)
  #:use-module (gnu workflows)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-1)
  #:export (guix-workflow))

(define (show-help)
  (for-each
   (lambda (line) (display line) (newline))
   '("Usage: guix workflow [OPTION]..."
     "Run multiple predefined computational process in a workflow"
     ""
     "  -i, --input=LOCATION   set LOCATION as input for a workflow"
     "  -o, --output=LOCATION  set LOCATION as output for a workflow"
     "  -e, --engine=ENGINE    set ENGINE for offloading to a cluster"
     "  -l, --list-available   list available processes"
     "  -p, --prepare=WORKFLOW Prepare to run WORKFLOW"
     "  -r, --run=WORKFLOW     Run WORKFLOW"
     "  -s, --search=REGEXP    search in synopsis and description using REGEXP"
     "  -g, --graph=WORKFLOW   Output the workflow in Dot-format"
     "  -w, --web-interface    Start the web interface"
     "  -h, --help             display this help and exit"
     "  -V, --version          display version information and exit"
     "")))

(define (show-available-workflows args)
  "Display available workflows."
  (format #t "Available workflows:~%")
  (let ((wfs (fold-workflows
              (lambda (p r)
                (if (string= (workflow-version p) "")
                    (vhash-cons (format #f "~a" (workflow-name p)) p r)
                    (vhash-cons (format #f "~a (~a)"
                                        (workflow-name p)
                                        (workflow-version p)) p r)))
              vlist-null)))
    (vlist-for-each (match-lambda
                      ((label . _)
                       (format #t "  * ~a~%" label)))
                    wfs))
  (newline))

(define %options
  ;; List of command-line options.
  (list (option '(#\h "help") #f #f
                (lambda args (show-help) (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args (show-version-and-exit "guix process")))
        (option '(#\e "engine") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'engine arg
                              (alist-delete 'engine result))))
        (option '(#\i "input") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'input arg
                              (alist-delete 'input result))))
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
        (option '(#\g "graph") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'query 'graph
                              (alist-cons 'value arg
                                          (alist-delete 'graph result)))))
        (option '(#\s "search") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'query 'search
                              (alist-cons 'value arg
                                          (alist-delete 'search result)))))
        (option '(#\l "list-available") #f #f
                (lambda args
                  (show-available-workflows args)))
        (option '(#\w "web-interface") #f #f
                (lambda args
                  (run-web-interface)))))

(define %default-options
  `((engine . "bash-engine")))

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
      ;; Handle searching for a process.
      ;; ----------------------------------------------------------------------
      ('search
       (match (find-workflows (assoc-ref opts 'value))
         (() #t)
         (matches
          (vlist-for-each (compose print-workflow-record cdr)
                          matches)))
       #t)
      ;; Handle preparing to running processes.
      ;; ----------------------------------------------------------------------
      ((and (or 'prepare 'run) action)
       ;; TODO: Deal with the situation wherein multiple processes
       ;; with the same name are defined.
       (let ((wf (match (find-workflow-by-name (assoc-ref opts 'value))
                   ((first . rest) first)
                   (_ #f)))
             (engine-name (assoc-ref opts 'engine))
             (wf-name (assoc-ref opts 'value)))
         (unless (and engine-name wf-name)
           (leave (G_ "Please provide --engine and --run arguments.~%")))
         (unless wf
           (leave (G_ "Cannot find a workflow with name ~s.~%") wf-name))
         (let ((engine (find-engine-by-name engine-name)))
           (unless engine
             (leave (G_ "The engine ~s is not available.~%") engine-name))
           ((case action
              ((prepare) workflow-prepare)
              ((run)     workflow-run))
            wf engine)))
       #t)
      ;; Handle running processes.
      ;; ----------------------------------------------------------------------
      ('graph
       (match (find-workflow-by-name (assoc-ref opts 'value))
         ((wf . rest)
          (format #t "~a\n" (workflow->dot wf)))
         (_ (leave (G_ "Could not find the workflow to graph.~%"))))
       #t)
      ;; Handle (or don't handle) anything else.
      ;; ----------------------------------------------------------------------
      (_ #t))))
