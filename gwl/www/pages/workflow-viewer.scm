;;; Copyright © 2016, 2017  Roel Janssen <roel@gnu.org>
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
  #:use-module (guix workflows)
  #:use-module (guix workflows graph)
  #:use-module (guix processes)
  #:use-module (guix packages)
  #:use-module (gnu workflows)
  #:use-module (gnu packages graphviz)
  #:use-module (guix store)
  #:use-module (ice-9 vlist)
  #:use-module (gwl www pages)
  #:use-module (gwl www config)
  #:export (page-workflow-viewer))

(define %daemon-connection #f)
(define (open-or-reuse-connection)
  (unless %daemon-connection
    (set! %daemon-connection (open-connection)))
  %daemon-connection)

(define (workflow-graph-svg-object workflow-name)
  "Return the SXML to render an SVG containing the graph of WORKFLOW-NAME."
  (let ((workflow-list (find-workflow-by-full-name workflow-name)))
    (if (null? workflow-list)
        `(p "Sorry, I could not render graph.")
        (let* ((workflow (car workflow-list))
               (store (open-or-reuse-connection))
               (dot-bin (string-append (package-output store graphviz) "/bin/dot"))
               (web-path (string-append "/static/graphs/" workflow-name ".svg"))
               (dot-file (string-append %www-root  "/static/graphs/" workflow-name))
               (svg-file (string-append dot-file ".svg")))
          (with-output-to-file dot-file
            (lambda _
              (display (workflow->dot workflow))))
          (system* dot-bin "-Tsvg" dot-file (string-append "-o" svg-file))
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
