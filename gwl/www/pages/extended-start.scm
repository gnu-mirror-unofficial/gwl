;;; Copyright © 2018  Roel Janssen <roel@gnu.org>
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

(define-module (gwl www pages extended-start)
  #:use-module (gwl www pages)
  #:use-module (syntax-highlight)
  #:use-module (syntax-highlight scheme)
  #:export (page-extended-start))

(define %guile-manual "https://www.gnu.org/software/guile/manual/html_node")

(define (page-extended-start request-path)
  (page-root-template "Guix Workflow Language" request-path
   `((h2 "Beyond started with the Guix workflow language")

     (h3 "Recap")

     (p "In the " (a (@ (href "/beyond-started")) "previous section") " we"
        " defined a workflow.  This section builds on the knowledge from the "
        "previous section, so if you haven't read that, now is the time to "
        (a (@ (href "/beyond-started")) "get beyond started") ".")

     (h3 "Extending workflows")

     (p "In the " (code "dynamic-workflow") " we create files and compressed"
        " those files.  In the following workflow we will delete those"
        " compressed files to learn how we can extend a workflow at any point"
        " in a new workflow.")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(highlights->sxml (highlight lex-scheme
"(define-module (extended-example-workflow)
  #:use-module (guix processes)
  #:use-module (guix workflows)
  #:use-module ((gnu packages compression) #:prefix package:)
  #:use-module (srfi srfi-1)
  #:use-module (example-workflow)) ; We are going to extend \"example-workflow\".

(define (delete-file-template filename)
  (process
   (name (string-append \"delete-file-\" (basename filename)))
   (run-time (complexity
              (space (megabytes 20))
              (time 10)))
   (procedure
    `(delete-file ,filename))))

(define-public extended-dynamic-workflow
  (let* (;; Get all processes of the other workflow.
         (foreign-processes (workflow-processes dynamic-workflow))

         ;; Get the processes that we want to extend on.
         (compress-file-processes (processes-filter-by-name
                                   foreign-processes \"compress-file\"))

         ;; Create the new processes.
         (delete-file-processes (map delete-file-template
                                     (map process-outputs
                                          compress-file-processes))))
    (workflow
     (name \"extended-dynamic-workflow\")
     (processes (append foreign-processes delete-file-processes))
     (restrictions
      (append
       (workflow-restrictions dynamic-workflow)
       (zip delete-file-processes compress-file-processes))))))")))))

     (p "With " (code "delete-file-template") " we created a function that"
        " returns a " (code "process") " that removes a file.  We use this"
        " function in " (code "extended-dynamic-workflow") " to run after"
        " each " (code "compress-file") " process.")

     (p "In the " (code "processes") " and " (code "restrictions") " fields we"
        " include the contents of " (code "dynamic-workflow") ".  Explicitly"
        " expressing the new " (code "restrictions") " displays how this"
        " workflow extends the other in a concise way.  Because "
        (code "dynamic-workflow") " is defined in a Scheme module, we can"
        " use the " (code "#:use-module") " facility to refer to it.")

     (h3 "Further reading")

     (p "The " (a (@ (href "https://www.gnu.org/software/guile/learn/"))
               "GNU Guile") " and "
               (a (@ (href "https://www.gnu.org/software/guix/manual/"))
                  "GNU Guix")
        " manuals are good places to learn the language and concepts on which "
        "GWL builds."))))
