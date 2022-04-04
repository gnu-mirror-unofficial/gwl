;;; Copyright © 2018-2022 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl-channel)
  #:export (gwl/devel))

(use-modules (ice-9 vlist)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-11)
             (guix packages)
             (guix transformations)
             (guix utils)
             (gnu packages base)
             (gnu packages autotools)
             (gnu packages package-management)
             (gnu packages pkg-config)
             (gnu packages gnupg)
             (gnu packages guile-xyz)
             (gnu packages graphviz)
             (gnu packages tex)
             (gnu packages texinfo)
             (gnu packages perl)
             (gnu packages rsync)
             (gnu packages ssh)
             (gnu packages version-control))

(define* (package-input-rewriting/spec* replacements
                                        #:key
                                        (deep? #t)
                                        (cut? (const #f)))
  "This is just like PACKAGE-INPUT-REWRITING/SPEC but takes an extra
argument CUT?, a procedure that takes the package value and
returns a boolean to determine whether rewriting should continue."
  (define table
    (fold (lambda (replacement table)
            (match replacement
              ((spec . proc)
               (let-values (((name version)
                             (package-name->name+version spec)))
                 (vhash-cons name (list version proc) table)))))
          vlist-null
          replacements))

  (define (find-replacement package)
    (vhash-fold* (lambda (item proc)
                   (or proc
                       (match item
                         ((#f proc)
                          proc)
                         ((version proc)
                          (and (version-prefix? version
                                                (package-version package))
                               proc)))))
                 #f
                 (package-name package)
                 table))

  (define replacement-property
    (gensym " package-replacement"))

  (define (rewrite p)
    (if (assq-ref (package-properties p) replacement-property)
        p
        (match (find-replacement p)
          (#f p)
          (proc
           (let ((new (proc p)))
             ;; Mark NEW as already processed.
             (package/inherit new
               (properties `((,replacement-property . #t)
                             ,@(package-properties new)))))))))

  (define (cut?* p)
    (or (assq-ref (package-properties p) replacement-property)
        (find-replacement p)
        (cut? p)))

  (package-mapping rewrite cut?*
                   #:deep? deep?))

(define guix-guile
  (and=> (assoc-ref (package-native-inputs guix) "guile") car))

(define with-guix-guile-instead-of-any-guile
  ;; Replace all the packages called "guile" with the Guile variant
  ;; used by the "guix" package.
  (package-input-rewriting/spec*
   `(("guile" . ,(const guix-guile)))
   #:deep? #false
   #:cut?
   (lambda (p)
     (not (or (string=? (package-name p) "gwl")
              (string-prefix? "guile-"
                              (package-name p)))))))

(define p
  with-guix-guile-instead-of-any-guile)

(define-public gwl/devel
  (package
    (inherit gwl)
    (source #f)
    (arguments
     '(#:make-flags
       '("GUILE_AUTO_COMPILE=0")))
    (inputs
     (list guix
           guix-guile
           (p guile-commonmark)
           (p guile-config)
           (p guile-drmaa)
           (p guile-gcrypt)
           (p guile-pfds)
           (p guile-syntax-highlight)
           (p guile-wisp)))
    (native-inputs
     (append
         (list autoconf automake pkg-config texinfo graphviz)
         (list
          coreutils
          ;; for make distcheck
          (texlive-updmap.cfg
           (list texlive-base
                 texlive-amsfonts))

          sed

          ;; For "make release"
          perl
          git-minimal

          ;; For manual post processing
          guile-lib
          rsync

          ;; For "git push"
          openssh)))))

gwl/devel
