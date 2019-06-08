;;; Copyright © 2018 Roel Janssen <roel@gnu.org>
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

(define-module (gwl www pages getting-started)
  #:use-module (gwl www pages)
  #:use-module (syntax-highlight)
  #:use-module (syntax-highlight scheme)
  #:export (page-getting-started))

(define %guile-manual "https://www.gnu.org/software/guile/manual/html_node")

(define (page-getting-started request-path)
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
          (pre (code (@ (class "bash")) "guix package -i gwl")))
        
     (h3 "Introduction")

     (p "In the GWL there are two concepts we need to know about: "
        (em "processes") " and " (em "workflows") ".  We describe a "
        "computation  (running a program, or evaluating a Scheme expression) "
        "using a " (code "process") ".  With a " (code "workflow") " we "
        "describe how multiple processes relate to each other (process "
        (code "B") " must run after process " (code "A") ", process " (code "C")
        " must run before process " (code "A") ").")

     (p "Running processes or workflows can be done programmatically using the "
        (code "process->script->run") " and " (code "workflow-run")
        " functions, or through the command-line by using the "
        (code "guix workflow") " command.")

     (p "To make processes and workflows available to Scheme and to the "
        "command-line, we write them as a "
        (a (@ (href ,(string-append %guile-manual "/Creating-Guile-Modules.html")))
           "Guile Scheme module") ".")

     (h3 "Example")

     (p "Let's start by writing the obligatory “Hello, world!” to familiarize"
        " with the components of the workflow.")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
            ,(highlights->sxml (highlight lex-scheme
"(define-module (example-workflow)
  #:use-module (gwl processes)
  #:use-module (gwl workflows))

(define-public hello-world
  (process
    (name \"hello-world\")
    (run-time (complexity
                (space   (megabytes 10))
                (time    10)  ; In seconds
                (threads 1))) ; 1 thread is the default.
    (procedure
     '(format #t \"Hello, world!~%\"))))")))))

     (p "With the " (code "define-module") " expression we tell "
        (a (@ (href "https://www.gnu.org/software/guile")) "GNU Guile")
        " interpreter that this is a Scheme module.")

     (p "After the " (code "define-module") " statement we've created a symbol "
        (code "hello-world") " that contains a " (code "process") " named"
        " ”hello-world” and a Scheme expression to display “Hello, world!”"
        " on our screen as the computational procedure.")

     (p "We also provided an upper-limit constraint on the space and time"
        " properties of the process using "  (code "run-time")"."
        " These limits may be enforced by the run-time engine, but it is not"
        " required to do so.  For example, when running the process with "
        (code "grid-engine") " these limits will be enforced by the job"
        " scheduler of your grid engine implementation, but when running the"
        " same process with " (code "simple-engine") " these resource limits"
        " are not enforced.")

     (h3 "Running programs")

     (p "But the “hello-world” doesn't justify building yet another workflow"
        " language.  When approaching the real world a little further, we use"
        " the software deployment strengths of " (a (@ (href "#")) "GNU Guix")
        " by summarizing the deployment of a program using a single Scheme "
        " symbol.")
     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
(span (@ (class "syntax-open")) "(")
(span (@ (class "syntax-special")) "define-module") " "
(span (@ (class "syntax-open")) "(") "example-workflow" (span (@ (class "syntax-close")) ")")
"
  #:use-module " (span (@ (class "syntax-open")) "(")
  "gwl processes" (span (@ (class "syntax-close"))")")
"
  #:use-module (gwl workflows)
" (strong "  #:use-module (gnu packages bioinformatics)") ")"
"

(" (span (@ (class "syntax-special")) "define-public") " samtools-index
  (process
    (name " (span (@ (class "syntax-string")) "\"samtools-index\"") ")
" (strong "    (package-inputs (list samtools))
    (data-inputs " (span (@ (class "syntax-string"))"\"/tmp/sample.bam\"") ")")
"
    (run-time (complexity
                (space (megabytes 500))
                (time  (hours 2))))
    (procedure
     " (strong "`(system (string-append " (span (@ (class "syntax-string"))
     "\"samtools index \"") " ,data-inputs" (span (@ (class "syntax-close")) "))"))
(span (@ (class "syntax-close")) ")))"))))

     (p "In the module " (code "(gnu packages bioinformatics)") " we can find"
        " the symbol " (code "samtools") " which will be added to the"
        " environment of the " (code "process") " so that we can be sure this"
        " program is available when running the process.")

     (p "It is important to list " (em "all") " packages required to run the"
        " process in the " (code "package-inputs") " field.")

     (p "For the newcomer to Scheme, the comma might seem misplaced.  However,"
        " notice the backquote (`) before " (code "system") "?  This is the "
        " syntax for a "
        (a (@ (href ,(string-append %guile-manual "/Expression-Syntax.html")))
           "quasiquote") ", and the seemingly misplaced comma"
        " is in on the plot.  As you might have guessed, the value of the "
        (code "data-inputs") " field will be put into the place of "
        (code ",data-inputs") " inside the " (code "system") " command.")

     (h3 (a (@ (href "http://www.gnu.org/philosophy/free-sw.html"))
            "Free Software") " all the way down")

     (p "Essentially, " (code "process-engines") " are a layer between "
        " the written Scheme code, and the running scripts.  Let's look"
        " at the " (code "grid-engine") " as an example.  If we "
        (em "prepare") " a workflow using: ")

     (div (@ (class "figure"))
          (pre (code (@ (class "bash"))
                     "guix workflow -p my-workflow.scm -e grid-engine")))

     (p "scripts will be generated for each process containing a command to schedule a job"
        " in the grid engine system:")

     (div (@ (class "figure"))
          (pre (code (@ (class "bash"))
               "qsub -N gwl-samtools-index  /gnu/store/"
               "01iadgakyqw702sal89j3raxwd84fdzr-samtools-index")))

     (p "If we look inside the file specified in the last argument, we find"
        " the job script with the grid engine-specific " (code "#$") " comments"
        " for the memory, time, threads, and the actual code that will run "
        " on the compute node once scheduling has been successful.")

     (p "The first line that will be executed in the script loads the proper"
        " environment for the remainder of the script to run:")

     (div (@ (class "figure"))
          (pre (code (@ (class "bash"))
               "source /gnu/store/yhbixc60bwyfa7k3hdw60z45zzn0k7lh-"
               "profile/etc/profile")))

     (p "The code generated from the original Scheme code can be inspected"
        " which enables us to debug, verify, and prototype fixes at a lower"
        " level than the Scheme code.")

     (p "The " (em "prepare") " option (" (code "-p") " switch) provides all"
        " the insights required to manually reproduce each step of the"
        " compute.")

     (h3 "Defining (dynamic) workflows")

     (p "On the " (a (@ (href "/beyond-started")) "next page") ", we will use"
        " templated processes and combine them in workflows.")

     (div (@ (style "text-align: center"))
          (div (@ (class "action-button"))
               (a (@ (href "/beyond-started"))
                  "Go beyond started"))))))
