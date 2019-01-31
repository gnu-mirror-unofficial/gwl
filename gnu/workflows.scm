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

(define-module (gnu workflows)
  #:use-module (gwl workflows)
  #:use-module ((guix utils)
                #:select (version>?))
  #:use-module ((guix ui)
                #:select (G_ warning warn-about-load-error))
  #:use-module ((guix combinators)
                #:select (fold2))
  #:use-module ((guix discovery)
                #:select (scheme-modules))
  #:use-module ((guix build syscalls)
                #:select (scandir*))
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  ;; For wisp support
  #:use-module (system base compile)
  #:use-module (language wisp spec)
  #:use-module (language tree-il optimize)
  #:use-module (language cps optimize)
  #:export (%workflow-module-path
            all-workflow-modules
            fold-workflows
            find-workflows
            find-workflow-by-name))

(define %workflow-module-path
  ;; Search path for process modules.  Each item must be either a directory
  ;; name or a pair whose car is a directory and whose cdr is a sub-directory
  ;; to narrow the search.
  (let* ((not-colon   (char-set-complement (char-set #\:)))
         (environment (string-tokenize (or (getenv "GUIX_WORKFLOW_PATH") "")
                                       not-colon)))
    (for-each (lambda (directory)
                ;; Put the workflow paths at the end because there are likely
                ;; only few modules to load workflows and more modules to load
                ;; other Guix-related stuff.  Putting the workflow path at the
                ;; end of the load path may therefore be faster at run-time.
                (set! %load-path
                      (append %load-path (list directory)))
                (set! %load-compiled-path
                      (append %load-compiled-path (list directory))))
              environment)
    (make-parameter environment)))

(define (wisp-suffix file)
  (cond ((string-suffix? ".w" file) ".w")
        ((string-suffix? ".wisp" file) ".wisp")
        ((string-suffix? ".gwl" file) ".gwl")))

;; This is the same as "scheme-files" in (guix discovery), except that
;; it looks for ".wisp"/".gwl"/".w" files.
(define* (wisp-files directory)
  "Return the list of Wisp files found under DIRECTORY, recursively.  The
returned list is sorted in alphabetical order.  Return the empty list if
DIRECTORY is not accessible."
  (define (entry-type name properties)
    (match (assoc-ref properties 'type)
      ('unknown
       (stat:type (lstat name)))
      ((? symbol? type)
       type)))

  ;; Use 'scandir*' so we can avoid an extra 'lstat' for each entry, as
  ;; opposed to Guile's 'scandir' or 'file-system-fold'.
  (fold-right (lambda (entry result)
                (match entry
                  (("." . _)
                   result)
                  ((".." . _)
                   result)
                  ((name . properties)
                   (let ((absolute (string-append directory "/" name)))
                     (case (entry-type absolute properties)
                       ((directory)
                        (append (wisp-files absolute) result))
                       ((regular)
                        (if (or (string-suffix? ".wisp" name)
                                (string-suffix? ".gwl" name)
                                (string-suffix? ".w" name))
                            (cons absolute result)
                            result))
                       ((symlink)
                        (cond ((or (string-suffix? ".wisp" name)
                                   (string-suffix? ".gwl" name)
                                   (string-suffix? ".w" name))
                               (cons absolute result))
                              ((stat absolute #f)
                               =>
                               (match-lambda
                                 (#f result)
                                 ((= stat:type 'directory)
                                  (append (wisp-files absolute)
                                          result))
                                 (_ result)))))
                       (else
                        result))))))
              '()
              (catch 'system-error
                (lambda ()
                  (scandir* directory))
                (lambda args
                  (let ((errno (system-error-errno args)))
                    (unless (= errno ENOENT)
                      (warning (G_ "cannot access `~a': ~a~%")
                               directory (strerror errno)))
                    '())))))


;; This is the same as "file-name->module-name" in (guix modules),
;; except that it looks for ".wisp" files.
(define wisp-file-name->module-name
  (let ((not-slash (char-set-complement (char-set #\/))))
    (lambda (file)
      "Return the module name (a list of symbols) corresponding to FILE."
      (map string->symbol
           (string-tokenize (basename file (wisp-suffix file)) not-slash)))))

;; Copied from Guile's (scripts compile) module.
(define (available-optimizations)
  (append (tree-il-default-optimization-options)
          (cps-default-optimization-options)))

;; Copied from Guile's (scripts compile) module.
(define (optimizations-for-level level)
  (let lp ((options (available-optimizations)))
    (match options
      (() '())
      ((#:partial-eval? val . options)
       (cons* #:partial-eval? (> level 0) (lp options)))
      ((kw val . options)
       (cons* kw (> level 1) (lp options))))))

(define* (wisp-modules directory #:optional sub-directory)
  "Return the list of Wisp modules available under DIRECTORY, and
compile them if necessary.  Optionally, narrow the search to
SUB-DIRECTORY."
  (define prefix-len
    (string-length directory))

  (filter-map (lambda (path)
                (let* ((file     (substring path prefix-len))
                       (compiled (string-append
                                  (dirname path) "/"
                                  (basename path (wisp-suffix path))
                                  ".go"))
                       (module   (wisp-file-name->module-name file)))
                  (catch #t
                    (lambda ()
                      (when (or (not (file-exists? compiled))
                                (let ((sc (stat compiled))
                                      (ss (stat path)))
                                  (> (stat:mtime ss)
                                     (stat:mtime sc))))
                        (format (current-error-port) "Compiling ~a..." path)
                        (force-output (current-error-port))
                        (compile-file path
                                      #:output-file compiled
                                      #:from wisp
                                      #:opts (optimizations-for-level 0))
                        (format (current-error-port) "OK\n")
                        (force-output (current-error-port))
                        (load-compiled compiled))
                      (resolve-interface module))
                    (lambda args
                      ;; Report the error, but keep going.
                      (warn-about-load-error module args)
                      #f))))
              (wisp-files (if sub-directory
                              (string-append directory "/" sub-directory)
                              directory))))

(define* (all-workflow-modules #:optional (path (%workflow-module-path)))
  "Return the list of workflow modules found in PATH, a list of directories to
search."
  (fold-right (lambda (spec result)
                (match spec
                  ((? string? directory)
                   (append (scheme-modules directory
                                           #:warn warn-about-load-error)
                           (wisp-modules directory)
                           result))
                  ((directory . sub-directory)
                   (append (scheme-modules directory sub-directory
                                           #:warn warn-about-load-error)
                           (wisp-modules directory sub-directory)
                           result))))
              '()
              path))

(define (fold-workflows proc init)
  "Call (PROC WORKFLOW RESULT) for each available workflow, using INIT as
the initial value of RESULT.  It is guaranteed to never traverse the
same workflow twice."
  (identity   ; discard second return value
   (fold2 (lambda (module result seen)
            (fold2 (lambda (var result seen)
                     (if (and (workflow? var)
                              (not (vhash-assq var seen)))
                         (values (proc var result)
                                 (vhash-consq var #t seen))
                         (values result seen)))
                   result
                   seen
                   (module-map (lambda (sym var)
                                 (false-if-exception (variable-ref var)))
                               module)))
          init
          vlist-null
          (all-workflow-modules))))

(define find-workflow-by-name
  (let ((workflows (delay
                     (fold-workflows (lambda (p r)
                                       (vhash-cons (workflow-name p) p r))
                                     vlist-null)))
        (version>? (lambda (p1 p2)
                     (version>? (workflow-version p1) (workflow-version p2)))))
    (lambda* (name #:optional version)
      "Return the list of workflows with the given NAME.  If VERSION is not #f,
then only return workflows whose version is prefixed by VERSION, sorted in
decreasing version order."
      (let ((matching (sort (vhash-fold* cons '() name (force workflows))
                            version>?)))
        (if version
            (filter (lambda (workflow)
                      (and=> (workflow-version workflow)
                             (cut string-prefix? version <>)))
                    matching)
            matching)))))

(define find-workflows
  (let ((workflows (delay
                     (fold-workflows (lambda (p r)
                                       (vhash-cons (workflow-name p) p r))
                                     vlist-null))))
    (lambda (keyword)
      "Return the list of workflows matching the given KEYWORD."
      (vlist-filter
       (match-lambda
         ((label . wf)
          (or (string-contains-ci (workflow-full-name wf) keyword)
              (string-contains-ci (workflow-synopsis wf) keyword)
              (string-contains-ci (workflow-description wf) keyword))))
       (force workflows)))))
