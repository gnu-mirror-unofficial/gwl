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

(define-module (gwl www pages beyond-started)
  #:use-module (gwl www pages)
  #:use-module (syntax-highlight)
  #:use-module (syntax-highlight scheme)
  #:export (page-beyond-started))

(define %guile-manual "https://www.gnu.org/software/guile/manual/html_node")

(define (page-beyond-started request-path)
  (page-root-template "Guix Workflow Language" request-path
   `((h2 "Beyond started with the Guix workflow language")

     (h3 "Recap")

     (p "In the " (a (@ (href "/getting-started")) "previous section") " we"
        " defined a Scheme module, a process, and we disected a grid engine "
        " job script to get to the details of how processes work.")

     (p "This section builds on the knowledge from the previous section, so"
        " if you haven't read that, now is the time to "
        (a (@ (href "/getting-started")) "get started") ".")

     (h3 "Defining workflows")

     (p "A " (em "workflow") " describes how " (em "processes") " relate to"
        " each other.  So before we can write the workflow, we must define"
        " some processes.  In the example we will create a file with a process"
        " named " (code "create-file") ", and we will compress that file"
        " using a process named " (code "compress-file") ".")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(highlights->sxml (highlight lex-scheme
"(define-module (example-workflow)
  #:use-module (gwl processes)
  #:use-module (gwl workflows)
  #:use-module (gnu packages compression)) ; For the \"gzip\" package.

(define-public create-file
  (process
    (name \"create-file\")
    (outputs \"/tmp/file.txt\")
    (run-time (complexity
                (space   (megabytes 20))
                (time    10)))
    (procedure
     `(call-with-output-file ,outputs
        (lambda (port)
          (format port \"~%\"))))))

(define-public compress-file
  (process
    (name \"compress-file\")
    (package-inputs (list gzip))
    (data-inputs \"/tmp/file.txt\")
    (outputs \"/tmp/file.txt.gz\")
    (run-time (complexity
                (space   (megabytes 20))
                (time    10)))
    (procedure
     `(system ,(string-append \"gzip \" data-inputs \" -c > \" outputs)))))")))))

     (p "With these definitions in place, we can run both in a single go by"
        " defining a workflow.")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(highlights->sxml (highlight lex-scheme
"(define-public file-workflow
  (workflow
    (name \"file-workflow\")
    ;; Include all processes that should run in the workflow.
    (processes (list create-file compress-file))
    (restrictions
     ;; Before we can compress the file, we must first create it.
     `((,compress-file ,create-file)))))")))))

     (h3 "Process templates")

     (p "We can make the inputs and outputs for a process variable, so that"
        " the same procedure can serve for multiple inputs and outputs."
        " Instead of writing a process directly, we can write a function that"
        " will return a process.  This is what it looks like:")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(highlights->sxml (highlight lex-scheme
"(define (compress-file input output)
  (process
    (name (string-append \"compress-file-\" (basename input)))
    (package-inputs (list gzip))
    (data-inputs input)
    (outputs output)
    (run-time (complexity
                (space   (megabytes 20))
                (time    10)))
    (procedure
     `(system ,(string-append \"gzip \" data-inputs \" -c > \" outputs)))))")))))

     (p "By using the " (code "define-dynamically") " function, we can now"
        "create multiple processes like this:")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(highlights->sxml (highlight lex-scheme
"(for-each (lambda (filename)
             (define-dynamically 
               ;; Create a unique symbol name.
               (string->symbol (string-append \"compress-file-\" (basename filename)))
               ;; Create a process using the template.
               (compress-file filename (string-append filename \".gz\"))))
           '(\"/tmp/one.txt\" \"/tmp/two.txt\" \"/tmp/three.txt\"))")))))

     (p "Which will create three symbols " (code "compress-file-one.txt") ", "
        (code "compress-file-two.txt") ", and " (code "compress-file-three.txt") ".")

     (h3 "Dynamic workflows")

     (p "This poses a potential problem workflows.  We would have to guess the"
        " dynamically generated symbol names, which isn't very dynamic."
        " Instead we can use Scheme's " (code "let") ", and " (code "map") " to"
        " do the work for us:")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(highlights->sxml (highlight lex-scheme
"(define-module (example-workflow)
  #:use-module (gwl processes)
  #:use-module (gwl workflows)
  ;; \"zip\" is both a package name and a function.  So we use a prefix
  ;; for packages to avoid this collision.
  #:use-module ((gnu packages compression) #:prefix package:)
  #:use-module (srfi srfi-1)) ; For the \"append\" and \"zip\" functions.

(define (create-file filename)
  (process
    (name (string-append \"create-file-\" (basename filename)))
    (outputs filename)
    (run-time (complexity
                (space   (megabytes 20))
                (time    10)))
    (procedure
     `(call-with-output-file ,outputs
        (lambda (port)
          (format port \"Hello, world!~%\"))))))

(define (compress-file input output)
  (process
    (name (string-append \"compress-file-\" (basename input)))
    (package-inputs (list package:gzip))
    (data-inputs input)
    (outputs output)
    (run-time (complexity
                (space   (megabytes 20))
                (time    10)))
    (procedure
     `(system ,(string-append \"gzip \" data-inputs \" -c > \" outputs)))))

(define-public dynamic-workflow
   (let* ((files '(\"/tmp/one.txt\" \"/tmp/two.txt\" \"/tmp/three.txt\"))
          (create-file-processes   (map create-file files))
          (compress-file-processes (map (lambda (filename)
                                         (compress-file filename (string-append filename \".gz\")))
                                        files)))
     (workflow
       (name \"dynamic-workflow\")
       (processes (append create-file-processes compress-file-processes))
       (restrictions
        (zip compress-file-processes create-file-processes)))))")))))

     (p "In GWL, we define restrictions explicitly.  This may seem redundant"
        " because GWL could compare the " (code "outputs") " field with the "
        (code "data-inputs") " to derive the restrictions.  However, taking"
        " this route, we rule out anything that isn't directly coupled this"
        " way.  Processes could also insert data in a database, which does not"
        " produce an output, in which case implicit restrictions fall short.")

     (p "Guile Scheme provides the utilities to express " (code "restrictions")
        " in a concise and clear way, like we've seen with " (code "zip") ".")

     (h3 "Reusing workflows in new workflows")

     (p "On the " (a (@ (href "/extended-start")) "next page") ", we will extend"
        " " (code "dynamic-workflow") " in a new workflow.")

     (div (@ (style "text-align: center"))
          (div (@ (class "action-button"))
               (a (@ (href "/extended-start"))
                  "Go beyond beyond started"))))))
