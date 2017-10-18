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

(define-module (gwl www pages getting-started defining-workflows)
  #:use-module (gwl www pages)
  #:export (page-getting-started-defining-workflows))

(define (page-getting-started-defining-workflows request-path)
  (page-root-template
   "Getting started with GWL" request-path
   `((h2 "Defining workflows")
     (p "Now that we've " (a (@ (href "/getting-started")) "had a lot of fun "
                             "defining " (em "processes"))
        ", we can move on and combine them in a " (em "workflow") ". "
        "Before we do that, we need to know how to write a "
        (em "Scheme module."))

     (h3 "Writing Scheme modules")

     (p "To write a Scheme module, we create a file that contains a call "
        "to " (code "(define-module ...)") ".  On the dots, we provide the "
        "name for the module, including the path to where it can be found.  "
        "Let's look at an example to see how this works.")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     "(define-module (my first workflow)
  #:use-module (guix processes)
  #:use-module (guix workflows)
  #:use-module (guix gexp))"))
          (p (strong "Figure 4") ": Defining a Scheme module."))

     (p "The first line " (code "(define-module (my first workflow))") " "
        "tells the Scheme interpreter (the program that runs this code) "
        "where we can expect to find this module.  So, in the case of "
        (code "my first workflow") " we should place that module in a "
        "file " (code "my/first/workflow.scm") ".  Then set the environment "
        "variable "  (code "GUIX_WORKFLOW_PATH") " to the path of this directory "
        "structure.  For example, when we place the " (code "workflow.scm")
        " file in " (code "/home/alice/my/first/") ", we must set "
        (code "GUIX_WORKFLOW_PATH") " to " (code "/home/alice") ", if we want "
        "GNU Guix to be able to find it.")

     (p "The lines starting with " (code "#:use-module") " import other Scheme "
        "modules.  For the Guix workflow language, we need the three specified "
        "in the example code: " (code "(guix processes)") " to define "
        (code "processes") ", " (code "(guix workflows)") " to define "
        (code "workflows") ", and " (code "guix gexp") " to write so-called "
        ;; TODO: Verify the URL.
        (a (@ (href "https://www.gnu.org/software/guix/documentation/index.html#G-Expressions"))
           "G-Expressions") ", which we use for running commands.")

     (p "Now that we've got the Scheme stuff out of the way, let's define a "
        "workflow!")
     
     (h3 "Defining workflows")

     (p "When the header has been defined, we can put our " (code "process")
        " and " (code "workflow") " descriptions in the file.  Instead of "
        "using " (code "(define ...)") ", we use " (code "(define-public ...)")
        ", so that the variable will be accessible to GNU Guix.")

     (p "So, before we can build a " (em "workflow") ", we need to define "
        "multiple " (em "process") "es.  To make it brief, we define a function "
        "to return a process, differing in a few parameters.")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     "(define (make-hello-process name output-path)
  (process
   (name name)
   (version \"1.0\")
   (output-path output-path)
   (run-time (complexity
              (space (* 1024 1024 5)) ; Five megabytes
              (time 3)))              ; Three seconds
   (procedure
    #~(with-output-to-file (string-append #$output-path \"/\" #$name \".txt\")
        (lambda _
          (display \"Hello you!\")
          (newline))))
   (synopsis \"A friendly greeter.\")
   (description \"This process has something to say.\")))"))
          (p (strong "Figure 5") ": A function to create process definitions."))

     (p "Notice how we use " (code "(define ...)") " instead of "
        (code "(define-public ...)") " for the function.")
     
     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     "(define-public hello-alice (make-hello-process \"hello-alice\" \"/home/alice\"))
(define-public hello-bob (make-hello-process \"hello-bob\" \"/home/bob\"))
(define-public hello-world (make-hello-process \"hello-world\" \"/home/world\"))"))
          (p (strong "Figure 6") ": Three process definitions."))

     (p "Now that we have three processes — " (code "hello-alice") ", "
        (code "hello-bob") ", and " (code "hello-world") " — we can "
        "specify how these should run in parallel in a " (code "workflow") ".")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     "(define-public hello-workflow
  (workflow
    (name \"hello-workflow\")
    (version \"1.0\")
    (processes `(,hello-alice ,hello-bob ,hello-world))
    (restrictions
     `((,hello-alice ,hello-world)
       (,hello-bob ,hello-world)))
    (synopsis \"A friendly greeting workflow.\")
    (description \"This workflow greets Alice, Bob and the world, making
sure that the world will be greeted first.\")))"))
          (p (strong "Figure 7") ": A workflow definition."))

     (p "This " (code "workflow") " can be run on your local computer using "
        "the following command: ")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
               "guix workflow --run=hello-workflow"))
          (p (strong "Command 5") ": Run the workflow on your local computer."))

     (p "Or, on a computing cluster that uses Grid Engine:")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
               "guix workflow --run=hello-workflow --engine=grid-engine"))
          (p (strong "Command 6") ": Run the workflow on a Grid cluster."))

     (p "A graphical overview of this workflow can be produced using "
        (code "guix workflow --graph=hello-workflow") " and is also available "
        "in the " (a (@ (href "/workflow-viewer")) "workflow viewer") ".")

     (h3 "Real-world usage")

     (p "The next page will consider running external programs, your own "
        "scripts, and defining processes based on a sample list — the things "
        "we need doing real-world stuff.")

     (div (@ (style "text-align: center"))
          (div (@ (class "action-button"))
               (a (@ (href "/getting-started/real-world-usage"))
                  "Real-world usage →")))

     ;; (p "There's a “real-world” workflow example available in the "
     ;;    (a (@ (href "https://github.com/UMCUGenetics/guix-performance-test"))
     ;;       "guix-performance-test") " repository.  The workflow language is "
     ;;       "an extension to " (a (@ (href "http://www.gnu.org/software/guix/"))
     ;;       "GNU Guix") ", which is written in "
     ;;       (a (@ (href "http://www.gnu.org/software/guile")) "GNU Guile Scheme")
     ;;       ".  Other than reading more, you can "
     ;;       (a (@ (href "mailto:R.R.E.Janssen-10@umcutrecht.nl"))
     ;;          "send me an e-mail") ".")

     ())
   #:dependencies '(highlight)))
