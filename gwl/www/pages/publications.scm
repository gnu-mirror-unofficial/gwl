;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl www pages publications)
  #:use-module (gwl www pages)
  #:export (page-publications))

(define (page-publications request-path)
  (page-root-template "Guix Workflow Language" request-path
   `((h1 "Publications")
     (ul
      (@ (class "publications"))
      (li (@ (class "year")
             (data-year "2021"))
          (div (@ (class "title"))
               (a (@ (href "https://fosdem.org/2021/schedule/event/guix_workflow/"))
                  "Guix Workflow Language: Extending a reproducible software deployment system for HPC"))
          (p (em "This is a FOSDEM 2021 talk by Ricardo Wurmus."))

          (p (strong "Abstract: ")
             "There are dozens of domain specific languages that allow
scientists to describe complex workflows. From the humble generic GNU
Make to large scale platforms like Apache Airflow you would think that
there is something there to satisfy everyone. All of these systems
have one thing in common: they have a strong focus on partitioning
large computations and scheduling work units, but when it comes to
managing the software environments that are the context of each of the
planned computations, they are often remarkably shy to offer
opinionated solutions.")
          (p "Software management and deployment often seems like an
afterthought. Workflow language designers increasingly seem to be
following the devops trend of resorting to opaque application bundles
to satisfy application and library needs. While this strategy has some
advantages it also comes with downsides that rarely seem to be weighed
carefully.")
          (p "We present the Guix Workflow Language — not as a
solution to the question of software deployment in HPC workflows, but
as an instance of convergent evolution: growing a workflow language
out of a generic reproducible software management and deployment
system (GNU Guix) instead of sprucing up a workflow language with
software deployment features. We hope to encourage a discussion about
the current state of workflow languages in HPC: when it comes to
software and distributed computations, are we approaching the peak or
do we circle a local maximum?"))
      
      (li (@ (class "year")
             (data-year "2019"))
          (div (@ (class "title"))
               (a (@ (href "https://link.springer.com/protocol/10.1007%2F978-1-4939-9074-0_24"))
                  "Scalable Workflows and Reproducible Data Analysis for Genomics"))
          (p "A chapter in the book " (a (@ (href "https://link.springer.com/book/10.1007/978-1-4939-9074-0"))
                                         (em "Evolutionary Genomics"))
             " by Francesco Strozzi " (em "et al.") " with a section
devoted to the Guix Workflow Language."))

      (li
       (div (@ (class "title"))
            (a (@ (href "https://archive.fosdem.org/2019/schedule/event/guixinfra/"))
               "GWL: GNU Workflow Language"))
       (p (em "FOSDEM 2019 talk by Ricardo Wurmus.")))

      (li (@ (class "year")
             (data-year "2017"))
          (div (@ (class "title"))
               (a (@ (href "https://archive.fosdem.org/2017/schedule/event/guixworkflowmanagement/"))
                  "Workflow management with GNU Guix"))
          (p (em "FOSDEM 2017 talk by Roel Janssen.")))))))
