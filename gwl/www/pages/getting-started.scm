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

     (p "On the " (a (@ (href "/beyond-started")) "next page") ", we
will see how we can combine more processes in a workflow.  We will
also use process templates to generate processes from a list of input
file names.")

     (div (@ (style "text-align: center"))
          (div (@ (class "action-button"))
               (a (@ (href "/beyond-started"))
                  "Go beyond started"))))))
