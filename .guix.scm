;;; Copyright © 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (guix build-system gnu)
             (guix packages)
             ((guix licenses) #:prefix license:)
             (gnu packages autotools)
             (gnu packages gnupg)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages graphviz)
             (gnu packages package-management)
             (gnu packages pkg-config)
             (gnu packages tex))

(define-public gwl
  (package
    (name "gwl")
    (version "0.1.1")
    (source #f)
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       '("GUILE_AUTO_COMPILE=0")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ;; This is only needed for make distcheck
       ("texlive" ,texlive-tiny)
       ("graphviz" ,graphviz)))
    (inputs
     `(("guile" ,guile-2.2)))
    (propagated-inputs
     `(("guix" ,guix)
       ("guile-commonmark" ,guile-commonmark)
       ("guile-gcrypt" ,guile-gcrypt)
       ("guile-pfds" ,guile-pfds)
       ("guile-syntax-highlight" ,guile-syntax-highlight)
       ("guile-wisp" ,guile-wisp)))
    (home-page "https://www.guixwl.org")
    (synopsis "Workflow management extension for GNU Guix")
    (description "This project provides two subcommands to GNU Guix and
introduces two record types that provide a workflow management extension built
on top of GNU Guix.")
    ;; The Scheme modules in guix/ and gnu/ are licensed GPL3+,
    ;; the web interface modules in gwl/ are licensed AGPL3+,
    ;; and the fonts included in this package are licensed OFL1.1.
    (license (list license:gpl3+ license:agpl3+ license:silofl1.1))))

gwl
