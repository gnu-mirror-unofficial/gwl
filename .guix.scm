;;; Copyright © 2018, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
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

(use-modules (guix packages)
             (gnu packages base)
             (gnu packages package-management)
             (gnu packages guile-xyz)
             (gnu packages tex)
             (gnu packages perl)
             (gnu packages ssh)
             (gnu packages version-control))

(define-public gwl/devel
  (package
    (inherit gwl)
    (source #f)
    (arguments
     '(#:make-flags
       '("GUILE_AUTO_COMPILE=0")))
    (inputs
     `(("guile-config" ,guile-config)
       ,@(package-inputs gwl)))
    (native-inputs
     `(("texlive" ,texlive-tiny) ; for make distcheck
       ("sed" ,sed)

       ;; For "make release"
       ("perl" ,perl)
       ("git" ,git-minimal)

       ;; For "git push"
       ("ssh" ,openssh)

       ,@(package-native-inputs gwl)))))

gwl/devel
