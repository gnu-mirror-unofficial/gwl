;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (config)
  #:use-module (config api)
  #:use-module (gwl process-engines)
  #:use-module (gwl web-interface)
  #:use-module (gwl workflows graph)
  #:use-module (gwl workflows)
  #:use-module (gwl utils)
  #:use-module (gwl ui)
  #:use-module (gwl config)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-37) #:prefix s37:)
  #:use-module (srfi srfi-1)
  #:export (guix-workflow))

(define *current-filename* (make-parameter #f))

;;;
;;; Entry point.
;;;

(define (guix-workflow . args)
  ;; We use the cdr here, because the first word will always be "guix"
  (let ((opts (getopt-config-auto (cdr (command-line)) config)))
    ;; Initialize %config
    (%config opts)

    (match (full-command opts)
      ((_)
       (format (current-error-port)
               "guix workflow: missing or unknown command name~%")
       (emit-help opts))
      ((_ "web")
       (let ((port (%config 'port)))
         (run-web-interface port)))
      ;; Handle running or preparing workflows.
      ((_ "run")
       (let* ((file-name (option-ref opts '(file)))
              (wf (parameterize ((*current-filename* file-name))
                    (load-workflow file-name)))
              (engine-name (option-ref opts 'engine)))
         (let ((engine (find-engine-by-name engine-name)))
           (unless engine
             (leave (G_ "The engine ~s is not available.~%") engine-name))
           (if (option-ref opts 'prepare)
               ;; Only prepare the workflow
               (workflow-prepare
                wf engine
                #:containerize?
                (option-ref opts 'container))

               ;; Run the workflow
               ;; TODO: see https://gitlab.com/a-sassmannshausen/guile-config/-/issues/15
               (let* ((options (s37:args-fold (cdr (command-line))
                                              (list (s37:option '(#\i "input") #t #f
                                                                (lambda (opt name arg result . rest)
                                                                  (apply values
                                                                         (alist-cons 'input arg result)
                                                                         rest))))
                                              (lambda (opt name arg result) #f) ; ignore
                                              (lambda (op loads) (cons op loads))
                                              '()))
                      (inputs (filter-map (match-lambda
                                            (('input . val) val)
                                            (_ #f))
                                          options)))
                 (workflow-run wf engine
                               #:inputs inputs
                               #:dry-run? (option-ref opts 'dry-run)
                               #:force? (option-ref opts 'force)
                               #:containerize?
                               (option-ref opts 'container)))))))
      ;; Handle workflow visualization
      ((_ "graph")
       (let* ((file-name (option-ref opts '(file))))
         (parameterize ((*current-filename* file-name))
           (match (load-workflow file-name)
             ((? workflow? wf)
              (format #t "~a\n" (workflow->dot wf)))
             (_ (leave (G_ "Failed to process the workflow.~%"))))))))))
