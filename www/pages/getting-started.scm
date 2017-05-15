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

(define-module (www pages getting-started)
  #:use-module (www pages)
  #:export (page-getting-started))

(define (page-getting-started request-path)
  (page-root-template
   "Getting started with GWL" request-path
   `((h2 "Getting started with the GWL")

     (h3 "Installation")
     (p "This tutorial assumes we have already "
        (a (@ (href "https://www.gnu.org/software/guix/manual/html_node/"
                    "Binary-Installation.html")) "installed GNU Guix") ". "
                    "Additionally, we need to install the "
                    (a (@ (href "https://git.roelj.com/guix/gwl.git"))
                       "GWL extension") ", which can be done with "
                       "the following commands:")

     (div (@ (class "figure"))
          (pre (code (@ (class "bash"))
                     "# Get the code
git clone https://git.roelj.com/guix/gwl.git && cd gwl/

# (Optional) Configure and compile the code
autoreconf -ivf && ./configure && make

# Add the 'workflow' and 'process' subcommands to GNU Guix
CURRENT_DIR=`pwd`
export GUILE_LOAD_PATH=\"$CURRENT_DIR${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH\""))
          (p (strong "Command 1") ": Get the workflow management extension "
             "for GNU Guix."))

     (p "After these steps have been completed, we can start using the "
        "workflow language.  In case you are wondering about which editor "
        "to use, anything that can edit text will do, but "
        (a (@ (href "https://www.gnu.org/software/emacs")) "GNU Emacs")
        " has excellent support for interactively running the code in "
        "the rest of the tutorial.")
     
     (h3 "Let's get started")
     (p "Before diving into the code samples, there are a "
        "few things we need to know about the workflow language.  First, "
        "there are two concepts we need to understand: " (em "processes")
        " and " (em "workflows") ".")

     (div (@ (class "figure")
             (style "width: 290pt; float: right;"))
       (img (@ (src "/static/images/workflows-processes.svg")
               (style "width: 100%")
               (alt "A graphical view of the relationship between "
                    "workflows and processes.")))
       (p (strong "Figure 1") ": A graphical view of the relationship between "
          "workflows and processes.  The arrows represent dependencies."))

     (p "A " (em "process") " is something you want to compute (by "
        "running a program, or a Scheme expression).  For example, "
        "calling structural variants using the program " (code "manta")
        " would be something to describe in a process.")
     (p "Each " (em "process") " can run on a different machine, so "
        "keeping " (em "processes") " small will help reduce the time "
        "it takes to run many of them in parallel.")
     (p "For combining multiple " (em "processes") " we can use a "
        (em "workflow") " which describes how a bunch of processes "
        "relate to each other (" (em "process A") " should run after "
        (em "process B") ", " (em "process C") " can run in parallel with "
        (em "process A") "). " (em "Figure 1") " displays how these "
        "concepts relate to each other.")

     (p (@ (style "clear: left"))
        "Secondly, the GWL is implemented in an existing language called "
        (em "Scheme") ".  This means it has all the benefits of a programming "
        "language, like code comments, functions and re-usable modules.  In "
        "fact, one of those re-usable modules is a package manager, which "
        "means the workflow definitions in this language automatically take "
        "care of installing all required programs to run all processes in "
        "the workflow.")

     (p "Without further ado, let's look at the definition of a "
        (code "process") ".")

     (h3 "Defining processes")
     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     "(define hello-world
  (process
   (name \"hello-world\")
   (version \"0.1\")
   (output-path \"/output/path/for/hello-world\")
   (run-time (complexity
              (space (* 1024 1024 5)) ; Five megabytes
              (time 3))) ; Three seconds
   (procedure
    #~(with-output-to-file (string-append #$output-path \"/hello-world.txt\")
        (lambda _ (format #t \"Hello world~%\"))))
   (synopsis \"A friendly greeter.\")
   (description \"This process has something to say to the world.\")))"))
          (p (strong "Figure 2") ": An example process definition."))

     (p "In the code displayed in " (em "Figure 2") " we defined a Scheme "
        "variable named " (code "hello-world") " to be a description of a "
        (code "process") " record type.  Apart from the "(em "name") ", "
        (em "version") ", " (em "synopsis") ", and " (em "description") " "
        "fields, we need to define the " (em "run-time complexity") " and the "
        (em "procedure") " to execute.")

     (h4 "Procedure")
     (p "The first line of the procedure starts with " (code "#~") ", which "
        "indicates we started a "
        (a (@ (href "https://www.gnu.org/software/guix/manual/html_node/G_002dExpressions.html"))
           "G-Expression") ".  In short, this means the code inside the "
           (code "#~()") " will be treated literally, unless it is explicitely "
           "unescaped with either " (code "#$") " or " (code "#$@") ".")

     (p "This allows us to write a template code in which we insert values we "
        "defined earlier.  For example: " (code "#~(begin (display #$name))")
        " would be translated into " (code "(begin (display \"hello-world\"))")
        ", when defined inside of the " (em "hello-world") " process.")

     (p "The template that remains after translating the variables into values "
        "is (or must be) valid Scheme code.  This code can be executed on the "
        "computing cluster, or your local machine.")
     
     (h4 "Run-time complexity")
     (p "The run-time complexity of a process describes how much time and how "
        "much memory is needed to run it.  We could just write static numbers, "
        "but then it could very well be that we are either drastically "
        "overestimating the run-time complexity of the actual process, or "
        "underestimating it.  To get the most out of a computing cluster, we "
        "need to come up with an estimate that is close to the actual usage.")

     (p "To come up with a good prediction of the resources we need, we can "
        "use all of the Scheme programming language, as long as the end result "
        "of the expression is a number representing the number of bytes to "
        "allocate for the space-complexity, and the number of seconds for the "
        "time-complexity.  For example, we could say “we need twice the input "
        "file size as space-complexity”.  This would be the Scheme expression "
        "to provide that size: "
        (code "(* 2 (stat:size (stat \"/path/to/input/file\")))") ".")

     (h4 "Running the process")
     (p "We could run the process defined in " (em "Figure 2") " with the "
        "command:")

     (div (@ (class "figure"))
             (pre (code (@ (class "bash"))
                        "guix process --run=hello-world
# Please run the following:

/gnu/store/28hzs2kvj3m27iivv0dxipwyipgq75bl-hello-world-0.1"))
             (p (strong "Command 2") ": Running a process (stage 1)."))

     (p "For " (code "guix") " to find our process definition, we need to add "
        "it to a Scheme module, and let " (code "guix") " find that module by "
        "adding it to the " (code "GUIX_WORKFLOW_PATH") " environment variable."
        "  We will learn how to do that in the second part of this tutorial.")

     (p "Instead of running it directly, we can specify an execution engine.  "
        "For example, we can let " (code "guix") " run processes on a "
        (em "Grid Engine") " cluster by running:")

     (div (@ (class "figure"))
          (pre (code (@ (class "bash"))
                     "guix process --run=hello-world --engine=grid-engine
Your job 7961546 (\"gi1k8r1dhbmwaxhyqgyqh4vc5y5ih6h7-hello-world-0.1\") has
been submitted"))
          (p (strong "Command 3") ": Using Grid Engine with GWL."))

     (p "Notice that the command to execute the script has changed to use "
        (code "qsub") " with which we can queue the job on a Grid Engine "
        " computing cluster.")
     
     (p "In the GWL " (em "processes") " are the building blocks for your
workflow definition.  Before we start to combine the building blocks to form
entire workflows, let's have a little more fun with them.")

     (h3 "Transforming hello-world")
     (p "So, we would like to run " (code "hello-world") ", but instead of
writing the output to " (code "/output/path/for/hello-world") ", we would like to
write it to " (code "/home/bob") ".  Do we have to duplicate the entire description?")
     (p "Fortunately, we don't.  We can use inheritance to make our changes:")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     ";; Note: You can override any property of 'hello-world'.
(define hello-bob
  (process (inherit hello-world)
           (name \"hello-bob\")
           (output-path \"/home/bob\")))"))
          (p (strong "Figure 3") ": Scheme code to reuse another process."))

     (p "And of course, we can turn the newly created " (code "hello-bob")
        " into a runnable script:")

     (div (@ (class "figure"))
       (pre (code (@ (class "bash"))
        "guix process --run=hello-bob
# Please run the following:

/gnu/store/6491my3r1sybzgkdgb6ngp2p1idg53pc-hello-bob-0.1"))
       (p (strong "Command 4") ": The command to run the inherited process."))

     (p "")

     (h3 "Defining workflows")
     (p "Now let's move on to part two: "
        (div (@ (style "text-align: center"))
          (div (@ (class "action-button"))
               (a (@ (href "/getting-started/defining-workflows"))
                  "Define workflows →")))))
   #:dependencies '(highlight)))
