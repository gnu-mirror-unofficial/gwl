;;; Copyright © 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl www pages tutorial)
  #:use-module (gwl www config)
  #:use-module (gwl www pages)
  #:use-module (syntax-highlight)
  #:use-module (syntax-highlight scheme)
  #:export (page-tutorial))

(define (page-tutorial request-path)
  (page-root-template "Guix Workflow Language" request-path
   `((h2 "Getting started with the Guix workflow language")

     (h3 "Installation")

     (p "This guide assumes " (a (@ (href "https://www.gnu.org/software/"
                                          "guix/manual/html_node/Binary-"
                                          "Installation.html")) "GNU Guix")
        " and " (a (@ (href "https://www.gnu.org/software/gwl")) "GNU GWL")
        " have been installed already.  In case the GNU GWL hasn't been"
        " installed, run:")

     (div (@ (class "figure"))
          (pre (code (@ (class "bash")) "guix install gwl")))

     (h3 "Introduction")

     (p "In the GWL there are two concepts we need to know
about: " (em "processes") " and " (em "workflows") ".  We describe a
computation (e.g. running a program) using a " (code "process") ".
With a " (code "workflow") " we describe how multiple processes relate
to each other (process " (code "B") " must run after
process " (code "A") ", process " (code "C") " must run before
process " (code "A") ").")

     (p "Processes and workflows are composed using a domain specific
language embedded in the general purpose language Scheme.  They can be
executed in order with the " (code "guix workflow") " command.")

     (h3 "Example")

     (p "Let's start by writing the obligatory “Hello, world!” to see
what a workflow might look like.")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
(span (@ (class "syntax-special")) "process") " hello-world
  # { echo \"Hello, world!\" }
")))

     (p "This text defines a " (code "process") " named ”hello-world”
which would run a shell snippet that prints “Hello, world!” to the
screen.  Delightful!")

     (h3 "Running programs")

     (p "But the “hello-world” doesn't justify building yet another
workflow language.  When approaching the real world a little further,
we use the software deployment strengths and reproducibility
guarantees of " (a (@ (href "#")) "GNU Guix") " by automating the
deployment of a potentially complex software environment using
the " (code "packages") " field.")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
"
" (span (@ (class "syntax-special")) "process") " samtools-index
  packages " (span (@ (class "syntax-string")) "\"samtools\"") "
  inputs " (span (@ (class "syntax-string")) "\"/tmp/sample.bam\"") "
  # {
    " (span (@ (class "syntax-string")) "samtools index {{") "inputs" (span (@ (class "syntax-string")) "}}") "
  }

" (span (@ (class "syntax-special")) "workflow") " do-the-thing
  processes samtools-index
")))

     (p "The " (code "packages") " field declares that we want
the " (code "samtools") " package to be available in the environment
of this process.  The package variant is fully determined by the
version of Guix used and is installed automatically when the process
is executed.  It is important to list " (em "all") " packages required
to run the process in the " (code "packages") " field.")

     (p "We also defined a simple workflow
named " (code "do-the-thing") " that executes just
the " (code "samtools-index") " process.")

     (p "In the next section, we will see how we can combine more
processes in a workflow.  We will also use process templates to
generate processes from a list of input file names.")

          (h3 "Defining workflows")

     (p "A " (em "workflow") " describes how " (em "processes") "
relate to each other.  So before we can write the workflow, we must
define some processes.  In this example we will create a file with a
process named " (code "create-file") ", and we will compress that file
using a process named " (code "compress-file") ".")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(with-input-from-file
                          (string-append (web-config 'examples-root)
                                         "/example-workflow1.w")
                        (lambda () (highlights->sxml (highlight lex-scheme)))))))

     (p "With these definitions in place, we can run both in one go by
defining a workflow.")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(highlights->sxml (highlight lex-scheme "\
workflow file-workflow
  processes
    auto-connect create-file compress-file")))))

     (p "The workflow specifies all processes that should run.
The " (code "auto-connect") " procedure links up all inputs and outputs of
all specified processes and ensures that the processes are run in the
correct order.  Later we will see other ways to specify process
dependencies.")

     (h3 "Process templates")

     (p "We can parameterize the inputs and outputs for a process, so
that the same process template can serve for different inputs and
outputs.  Here is a process template that is parameterized
on " (code "input") ":")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(highlights->sxml (highlight lex-scheme "\
process compress-file (with input)
  name
    string-append \"compress-file-\"
                  basename input
  packages \"gzip\"
  inputs input
  outputs
    string-append input \".gz\"
  run-time
    complexity
      space 20 mebibytes
      time  10 seconds
  # {
    gzip {{input}} -c > {{outputs}}
  }")))))

     (h3 "Dynamic workflows")

     (p "We can now dynamically create compression processes by
instantiating the " (code "compress-file") " template with specific
input file names.  We use Scheme's " (code "let") ",
and " (code "map") " to simplify the work for us:")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(with-input-from-file
                          (string-append (web-config 'examples-root)
                                         "/example-workflow.w")
                        (lambda () (highlights->sxml (highlight lex-scheme)))))))

     (p "In the GWL, we can define process dependencies explicitly.
This is useful when processes don't have explicit " (code "outputs") "
or " (code "inputs") ".  Processes can do something other than
producing output files, such as inserting data in a database, so
process dependencies can be specified manually.")

     (p "Restrictions can be specified as an association list mapping
processes to their dependencies, or via the
convenient " (code "graph") " syntax.")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(highlights->sxml (highlight lex-scheme "\
workflow graph-example
  processes
    graph
      A -> B C
      B -> D
      C -> B")))))

     (h3 "Extending workflows")

     (p "In the " (code "dynamic-workflow") " we created files and
compressed them.  In the following workflow we will generate a file
containing some information about these compressed files to learn how
we can extend a workflow at any point in a new workflow.")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ,(with-input-from-file
                          (string-append (web-config 'examples-root)
                                         "/extended-example-workflow.w")
                        (lambda () (highlights->sxml (highlight lex-scheme)))))))

     (p "With " (code "list-file-template") " we created a procedure
that returns a " (code "process") " that generates a file containing
details about the compressed archive.  We use this function
in " (code "extended-dynamic-workflow") " to run after
each " (code "compress-file") " process.")

     (p "In the " (code "processes") " field we include the contents
of " (code "dynamic-workflow") ", thereby concisely extending it.")

     (h3 "Further reading")

     (p "The "
        (a (@ (href "https://workflows.guix.info/manual/"))
           "GWL manual")
        " tries to cover everything you will need to know to write
real-world scientific workflows with the GWL.")

     (p "The "
        (a (@ (href "https://www.gnu.org/software/guile/learn/"))
           "GNU Guile")
        " and "
        (a (@ (href "https://www.gnu.org/software/guix/manual/"))
           "GNU Guix")
        " manuals are good places to learn the language and concepts
on which GWL builds."))))
