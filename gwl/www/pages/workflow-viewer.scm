;;; Copyright © 2016, 2017  Roel Janssen <roel@gnu.org>
;;; Copyright © 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (gwl www pages workflow-viewer)
  #:use-module (gwl config)
  #:use-module (gwl utils)
  #:use-module (gwl workflows)
  #:use-module (gwl workflows graph)
  #:use-module (gwl www pages)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module ((web uri) #:select (uri-decode))
  #:export (page-workflow-viewer))

(define *current-filename* (make-parameter #f))

(define %all-workflows
  (let* ((dir (or (%config 'workflows-directory)
                  (%config 'examples-root-directory)))
         (files (filter (lambda (file)
                          (or (wisp-suffix file)
                              (string-suffix? ".scm" file)))
                        (scandir dir))))
    (filter workflow? (map (lambda (file)
                             (parameterize ((*current-filename* file))
                               (load-workflow (string-append dir "/" file))))
                           files))))

(define (find-workflow-by-name name version)
  (find (lambda (wf)
          (and (string=? (workflow-name wf) name)
               (or (and=> (workflow-version wf)
                          (lambda (v) (string=? v version)))
                   #t)))
        %all-workflows))

(define (workflow-graph-svg-object workflow-name version)
  "Return the SXML to render an SVG containing the graph of WORKFLOW-NAME."
  (match (find-workflow-by-name workflow-name version)
    (#f `(p "Sorry, there is no matching workflow."))
    (workflow
     (let* ((dot-file (string-append (%config 'assets-directory)
                                     "/graphs/" workflow-name ".dot"))
            (svg-file (string-append (dirname dot-file) "/"
                                     (basename dot-file ".dot") ".svg"))
            (web-path (string-append "/static/graphs/" (basename svg-file))))
       (unless (file-exists? svg-file)
         (with-output-to-file dot-file
           (lambda _
             (display (workflow->dot workflow))))
         (system* (%config 'dot) "-Tsvg" dot-file
                  (string-append "-o" svg-file)))
       `(img (@ (src ,web-path)
                (style "max-height: 100%; max-width: 100%")))))))

(define page-workflow-viewer
  (let* ((workflows (fold
                     (lambda (wf acc)
                       (cons (cons (workflow-name wf)
                                   (workflow-version wf))
                             acc))
                     '() %all-workflows))
         (num-workflows (length workflows)))
    (lambda* (request-path #:key (post-data ""))
      (page-root-template
       "Guix Workflow Language" request-path
       `((h1 "Workflow viewer")
         (p "There " ,(if (> num-workflows 1) "are " "is ")
            ,num-workflows " available workflows.  Please choose one below.")
         (form (@ (action "/workflow-viewer")
                  (method "POST"))
               (select (@ (name "workflow")
                          (onchange "this.form.submit()"))
                       (option (@ (value "None")) "Please choose a workflow")
                       ,(map
                         (match-lambda
                           ((name . #f)
                            `(option (@ (value ,name)) ,name))
                           ((name . version)
                            `(option (@ (value ,name "@" ,version))
                                     ,(format #f "~a (~a)"
                                              name version))))
                         workflows))
               ,(match (string-split post-data #\=)
                  (("workflow" name+version)
                   (let* ((name+version (uri-decode name+version))
                          (version-index (string-index-right name+version #\@))
                          (name (if version-index
                                    (substring name+version 0 version-index)
                                    name+version))
                          (version (if version-index
                                       (substring name+version (1+ version-index))
                                       #f)))
                     `(div (@ (id "workarea"))
                           ,(workflow-graph-svg-object name version))))
                  (_ '()))))))))
