;;; Copyright © 2016, 2017  Roel Janssen <roel@gnu.org>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gnu workflows)
  #:use-module (gwl processes)
  #:use-module (gwl workflows)
  #:use-module (gwl workflows graph)
  #:use-module (gwl www pages)
  #:use-module (gwl www config)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:export (page-workflow-viewer))

(define (workflow-graph-svg-object workflow-name)
  "Return the SXML to render an SVG containing the graph of WORKFLOW-NAME."
  (match (find-workflow-by-full-name workflow-name)
    (() `(p "Sorry, I could not render graph."))
    ((workflow . _)
     (let* ((web-path (string-append "/static/graphs/" workflow-name ".svg"))
            (dot-file (string-append (web-config 'static-root) "/graphs/" workflow-name))
            (svg-file (string-append dot-file ".svg")))
       (unless (file-exists? svg-file)
         (with-output-to-file dot-file
           (lambda _
             (display (workflow->dot workflow))))
         (system* (web-config 'dot) "-Tsvg" dot-file (string-append "-o" svg-file)))
       `(img (@ (src ,web-path)
                (style "max-height: 100%; max-width: 100%")))))))

(define* (page-workflow-viewer request-path #:key (post-data ""))
  (let* ((workflows (fold-workflows
                     (lambda (p r)
                       (vhash-cons (workflow-full-name p) p r))
                     vlist-null))
         (num-workflows (vlist-length workflows)))
    (page-root-template "Guix Workflow Language" request-path
     `((h2 "Workflow viewer")
       (p "There " ,(if (> num-workflows 1) "are " "is ")
          ,num-workflows " available workflows.  Please choose one below.")
       (form (@ (action "/workflow-viewer")
                (method "POST"))
             (select (@ (name "workflow")
                        (onchange "this.form.submit()"))
                     (option (@ (value "None")) "Please choose a workflow")
                     ,(vlist->list
                       (vlist-map
                        (lambda (pair)
                          `(option (@ (value ,(workflow-full-name (cdr pair))))
                                   ,(string-append (workflow-name (cdr pair)) " @ "
                                                   (workflow-version (cdr pair)))))
                        workflows)))
             (p "")
             ,(if (string= post-data "")
                  '()
                  (let ((name (cadr (string-split post-data #\=))))
                  `(div (@ (id "workarea"))
                        ,(workflow-graph-svg-object name)))))))))
