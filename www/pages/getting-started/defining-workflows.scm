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

(define-module (www pages getting-started defining-workflows)
  #:use-module (www pages)
  #:export (page-getting-started-defining-workflows))

(define (page-getting-started-defining-workflows request-path)
  (page-root-template
   "Getting started with GWL" request-path
   `((h2 "Defining workflows")
     (p "Now that we've " (a (@ (href "/getting-started")) "had a lot of fun "
                             "defining " (em "processes"))
        ", we can move on and combine them in a " (em "workflow") ". "
        "Before we do that, we need to know how to write a " (em "Scheme module."))

     (h3 "Writing Scheme modules")
     (p "Scheme modules are files that start with the following lines:")

     (pre (code (@ (class "scheme"))
          "(define-module (my workflow stuff)
  #:use-module (guix processes)
  #:use-module (guix workflows)
  #:use-module (guix gexp))"))

     (p "The first line " (code "(define-module (my workflow stuff))") " "
        "tells the Scheme interpreter (the program that runs this code) "
        "where we can expect to find this module.  So, in the case of "
        (code "my workflow stuff") " we should place that module in a "
        "file " (code "my/workflow/stuff.scm") ".  Then set the environment "
        "variable "  (code "GUIX_WORKFLOW_PATH") " to the path of this directory "
        "structure.  For example, when we place the " (code "stuff.scm")
        " file in " (code "/home/alice/my/workflow/") ", we should set "
        (code "GUIX_WORKFLOW_PATH") " to " (code "/home/alice") ".")

     (h3 "Defining workflows")

     (p "When the header has been defined, we can put our process and workflow "
        "descriptions in the file.  Instead of using " (code "(define ...)")
        ", we use " (code "(define-public ...)") ", so that the variable will "
        "be accessible when using the module in another place.")

     (p "So, before we can build a " (em "workflow") ", we need to define "
        "multiple " (em "processes") ":")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                     "(define-public 
  (process (inherit hello-world)
           (name \"hello-bob\")
           (output-path \"/home/bob\")))"))
          (p (strong "Figure 3") ": Scheme code to reuse another process."))

     )

   #:dependencies '(highlight)))
