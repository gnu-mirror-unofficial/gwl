;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Roel Janssen <roel@gnu.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix scripts process)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix utils)
  #:use-module (guix processes)
  #:use-module (gnu processes)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-1)
  #:export (guix-process))

(define (show-help)
  (display (_ "Usage: guix process [OPTION]...
Run a predefined computational process.\n"))
  (display (_ "
  -i, --input=LOCATION   set LOCATION as input for a workflow"))
  (display (_ "
  -o, --output=LOCATION  set LOCATION as output for a workflow"))
  (display (_ "
  -e, --engine=ENGINE    set ENGINE for offloading to a cluster"))
  ;; (display (_ "
  ;; -l, --list-available-engines
  ;;                        list available engines for offloading"))
  (display (_ "
  -p, --list-available   list available processes"))
  (display (_ "
  -r, --run=PROCESS      Run PROCESS."))
  (display (_ "
  -s, --search=REGEXP    search in synopsis and description using REGEXP."))
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline))

(define (show-available-processes args)
  "Display available processes."
  (format #t "Available processes:~%")
  (let ((processes (fold-processes
                     (lambda (p r)
                       (vhash-cons (process-full-name p) p r))
                     vlist-null)))
    (vlist-for-each (lambda (pair)
                      (format #t "  * ~a~%" (car pair)))
                    processes))
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
        (option '(#\r "run") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'query 'run
                              (alist-cons 'value arg
                                          (alist-delete 'run result)))))
        (option '(#\s "search") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'query 'search
                              (alist-cons 'value arg
                                          (alist-delete 'search result)))))
        (option '(#\p "list-available") #f #f
                (lambda args
                  (show-available-processes args)))))

(define %default-options
  `((engine . "bash-engine")))

;;;
;;; Entry point.
;;;

(define (guix-process . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (when (assq 'argument result)
                    (leave (_ "~A: extraneous argument~%") arg))

                  (alist-cons 'argument arg result))
                %default-options))

  (let ((opts (parse-options)))
    (match (assoc-ref opts 'query)
      ;; Handle searching for a process.
      ;; ----------------------------------------------------------------------
      ('search
       (let* ((procs (find-processes (assoc-ref opts 'value))))
         (unless (null? procs)
           (vlist-for-each (lambda (proc)
                       (print-process-record (cdr proc) #t)) procs)))
       #t)
      ;; Handle running a process.
      ;; ----------------------------------------------------------------------
      ('run
       ;; TODO: Deal with the situation wherein multiple processes
       ;; with the same name are defined.
       (let* ((procs (find-process-by-name (assoc-ref opts 'value)))
              (proc (if (null? procs) '() (car procs)))
              (engine-name (assoc-ref opts 'engine))
              (proc-name (assoc-ref opts 'value)))
         ;; Use "bash" as the default engine.
         (when (eq? engine-name #f) (set! engine-name "bash"))
         (if (eq? proc-name #f)
             (format #t "Please provide --engine and --run arguments.~%")
             (if (not (process? proc))
                 (format #t "Cannot find a process with name ~s.~%" proc-name)
                 (let ((engine-symbol
                        (module-ref
                         (resolve-interface
                          (append '(guix process-engines)
                                  (list (string->symbol engine-name))))
                         (string->symbol engine-name))))
                   (if (not engine-symbol)
                       (format #t "The engine ~s is not available." engine-name)
                       (process->script proc engine-symbol))))))
       #t)
      ;; Handle (or don't handle) anything else.
      ;; ----------------------------------------------------------------------
      (#f #t))))
