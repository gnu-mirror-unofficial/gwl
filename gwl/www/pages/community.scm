;;; Copyright © 2016, 2017  Roel Janssen <roel@gnu.org>
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

(define-module (gwl www pages community)
  #:use-module (gwl www pages)
  #:export (page-community))

(define (page-community request-path)
  (page-root-template "Guix Workflow Language" request-path
   `((h1 "Get in Touch!")
     (p "For real-time support from the community, you can connect to
the " (code "#guix-hpc") " channel on irc.freenode.net.  There you can
get help about anything related to GNU Guix and the Guix Workflow
Language in an HPC context.

The " (code "#guix-hpc") " channel is logged. Previous conversations
can be browsed online. See the "
(a (@ (href "https://logs.guix.gnu.org/guix-hpc")) "channel logs") ".")
     (p "Long-form discussions are best held on the " (a (@ (href "mailto:gwl-devel@gnu.org")) "mailing list") ".")
     (p "You can " (a (@ (href "https://lists.gnu.org/mailman/listinfo/gwl-devel")) "browse the list archives here") "."))))
