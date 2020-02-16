;;; Copyright © 2016, 2017  Roel Janssen <roel@gnu.org>
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl www pages welcome)
  #:use-module (gwl www pages)
  #:export (page-welcome))

(define (page-welcome request-path)
  (page-root-template "Guix Workflow Language" request-path
   `((h2 "A workflow management language extension for GNU Guix")

     (p "The Guix Workflow Language (GWL) provides a scientific
computing extension to GNU Guix's "
        (a (@ (href "https://arxiv.org/abs/1305.4584"))
           "declarative language for package management")
        " for the declaration of scientific workflows.")

     (p "It combines the specification of work units and their
relationship to one another with the reproducible software deployment
facilities of the functional package manager GNU Guix.  A GWL workflow
will always run in a reproducible environment that GNU Guix
automatically prepares.  The GWL extends your Guix installation with a
single new sub-command: " (code "guix workflow") ".")

     (p "In the GWL there are two concepts we need to know about:
processes and workflows.  We describe a computation (running a
program, or evaluating a Scheme expression) using a process.  A
workflow describes how individual processes relate to each
other (e.g. "
        (em "process B must run after process A, and process C must
run before process A")
        ").")

     (p "GWL workflows are executable code.  The workflow language is
embedded in the powerful general purpose language "
        (a (@ (href "https://gnu.org/software/guile/"))
           "Guile Scheme")
        ", so you can compute arbitrarily complex process and workflow
definitions.  The GWL supports a classic Lisp syntax as well as a
Python-like syntax called "
        (a (@ (href "https://www.draketo.de/light/english/wisp-lisp-indentation-preprocessor"))
           "Wisp") ".")

     (div (@ (style "text-align: center;"))
          (div (@ (class "action-button"))
               (a (@ (href "/tutorial"))
                  "Get started with a tutorial"))))))
