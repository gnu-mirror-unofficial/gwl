;;; Copyright © 2018  Roel Janssen <roel@gnu.org>
;;; Copyright © 2019  Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gwl www config)
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
                     ,(with-input-from-file
                          (string-append (web-config 'examples-root)
                                         "/extended-example-workflow.scm")
                        (lambda () (highlights->sxml (highlight lex-scheme)))))))

     (p "With " (code "delete-file-template") " we created a function that"
        " returns a " (code "process") " that removes a file.  We use this"
        " function in " (code "extended-dynamic-workflow") " to run after"
        " each " (code "compress-file") " process.")

     (p "In the " (code "processes") " field we include the contents
of " (code "dynamic-workflow") ", thereby concisely extending it.
Because " (code "dynamic-workflow") " is defined in a Scheme module,
we can use the " (code "#:use-module") " facility to refer to it.")

     (h3 "Further reading")

     (p "The " (a (@ (href "https://www.gnu.org/software/guile/learn/"))
               "GNU Guile") " and "
               (a (@ (href "https://www.gnu.org/software/guix/manual/"))
                  "GNU Guix")
        " manuals are good places to learn the language and concepts on which "
        "GWL builds."))))
