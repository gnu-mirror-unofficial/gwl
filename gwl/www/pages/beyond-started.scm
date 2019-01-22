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

(define-module (gwl www pages beyond-started)
  #:use-module (gwl www config)
  #:use-module (gwl www pages)
  #:use-module (syntax-highlight)
  #:use-module (syntax-highlight scheme)
  #:export (page-beyond-started))

(define %guile-manual "https://www.gnu.org/software/guile/manual/html_node")

(define (page-beyond-started request-path)
  (page-root-template "Guix Workflow Language" request-path
   `((h2 "Beyond started with the Guix workflow language")

     (h3 "Recap")

     (p "In the " (a (@ (href "/getting-started")) "previous
section") " we defined a Scheme module, a process, and we disected a
grid engine job script to get to the details of how processes work.")

     (p "This section builds on the knowledge from the previous
section, so if you haven't read that, now is the time
to " (a (@ (href "/getting-started")) "get started") ".")

     (h3 "Defining workflows")

     (p "A " (em "workflow") " describes how " (em "processes") "
relate to each other.  So before we can write the workflow, we must
define some processes.  In the example we will create a file with a
process named " (code "create-file") ", and we will compress that file
using a process named " (code "compress-file") ".")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(with-input-from-file
                          (string-append (web-config 'examples-root)
                                         "/example-workflow1.scm")
                        (lambda () (highlights->sxml (highlight lex-scheme)))))))

     (p "With these definitions in place, we can run both in a single
go by defining a workflow.")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(highlights->sxml (highlight lex-scheme "\
(define-public file-workflow
  (workflow
    (name \"file-workflow\")
    (processes (auto-connect create-file compress-file))))")))))

     (p "The workflow specifies all processes that should run.
The " (code "auto-connect") " procedure links up all inputs and outputs of
all specified processes and ensures that the processes are run in the
correct order.  Later we will see other ways to specify process
dependencies.")

     (h3 "Process templates")

     (p "We can make the inputs and outputs for a process variable, so
that the same procedure can serve for multiple inputs and outputs.
Instead of writing a process directly, we can write a function that
returns a process.  This is what it looks like:")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(highlights->sxml (highlight lex-scheme "\
(define (compress-file input)
  (process
    (name (string-append \"compress-file-\" (basename input)))
    (package-inputs (list gzip))
    (data-inputs (list input))
    (outputs (list (string-append input \".gz\")))
    (run-time (complexity
                (space   (megabytes 20))
                (time    10)))
    (procedure
     `(system ,(string-append \"gzip \" (first data-inputs)
                              \" -c > \" (first outputs))))))")))))

     (h3 "Dynamic workflows")

     (p "We can now dynamically create compression processes by
applying the " (code "compress-file") " procedure to input and output
file names.  We use Scheme's " (code "let") ", and " (code "map") " to
simplify the work for us:")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(with-input-from-file
                          (string-append (web-config 'examples-root)
                                         "/example-workflow.scm")
                        (lambda () (highlights->sxml (highlight lex-scheme)))))))

     (p "In GWL, we can define process dependencies explicitly.  This
is useful when processes don't have explicit " (code "outputs") "
or " (code "data-inputs") ".  Processes can do something other than
producing output files, such as inserting data in a database, so
process dependencies can be specified manually.")

     (p "Restrictions can be specified as an association list mapping
processes to their dependencies, or via the
convenient " (code "graph") " syntax.")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(highlights->sxml (highlight lex-scheme "\
(workflow
 (name \"graph-example\")
 (processes
  (graph (A -> B C)
         (B -> D)
         (C -> B))))")))))

     (h3 "Reusing workflows in new workflows")

     (p "On the " (a (@ (href "/extended-start")) "next page") ", we
will extend " (code "dynamic-workflow") " in a new workflow.")

     (div (@ (style "text-align: center"))
          (div (@ (class "action-button"))
               (a (@ (href "/extended-start"))
                  "Go beyond beyond started"))))))
