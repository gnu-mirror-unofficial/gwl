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
             (gnu packages package-management)
             (gnu packages gnupg)
             (gnu packages guile-xyz)
             (gnu packages tex)
             (gnu packages perl)
             (gnu packages rsync)
             (gnu packages ssh)
             (gnu packages version-control))

(use-modules
 ((guix licenses) #:prefix license:)
 (guix download)
 (guix git-download)
 (guix build-system gnu)
 (gnu packages)
 (gnu packages autotools)
 (gnu packages base)
 (gnu packages mes)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (gnu packages parallel))

;; Later versions (such as 1.03.6) have a regression so that no
;; #define'd symbols are recorded.
(define-public nyacc-older
  (package
    (inherit nyacc-0.99)
    (version "1.03.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/nyacc/nyacc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1vdiqpm3p0ndmpmkzcpkpjvgklfsk4wxrhkixdxbczpafdfl635p"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "configure"
                    (("GUILE_GLOBAL_SITE=\\$prefix.*")
                     "GUILE_GLOBAL_SITE=\
$prefix/share/guile/site/$GUILE_EFFECTIVE_VERSION\n"))
                  #t))))
    (inputs
     `(("guile" ,guile-3.0)))))

(define-public guile-drmaa
  (let ((commit "0992a6f9404bf2e5ec6b64bcaf337db42b1d5051")
        (revision "1"))
    (package
      (name "guile-drmaa")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.elephly.net/software/guile-drmaa.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "03bvakj1877nx59zdpw2y7wy2kw8wvq3ps7kbmgvja89lbgvypjz"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("pkg-config" ,pkg-config)
         ("texinfo" ,texinfo)
         ("sed" ,sed)))
      (inputs
       `(("guile" ,guile-3.0)))
      (propagated-inputs
       `(("guile-bytestructures" ,guile-bytestructures)
         ("nyacc" ,nyacc-older)))
      (home-page "https://git.elephly.net/software/guile-drmaa")
      (synopsis "Guile bindings for the DRMAA scheduler library")
      (description "This package provides Guile bindings to the DRMAA
HPC scheduler library to submit jobs to HPC batch processing systems,
and to control submitted jobs.")
      (license license:gpl3+))))

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

;; The tests throw exceptions with Guile 3.0.5, because they evaluate
;; (exit ...).
(define-public guile-commonmark/fixed
  (package
    (inherit guile-commonmark)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests-when-building-with-guile-3.0.5
           (lambda _
             (substitute* (find-files "tests" "\\.scm$")
               (("\\(exit.*") ""))
             #t)))))))

;; It's not just the GWL that uses guile-commonmark, so we need to
;; change it throughout the input graph.
(define with-fixed-commonmark
  (package-input-rewriting/spec*
   `(("guile-commonmark" . ,(const guile-commonmark/fixed)))
   #:deep? #false
   #:cut?
   (lambda (p)
     (not (or (string=? (package-name p) "gwl")
              (string-prefix? "guile-"
                              (package-name p)))))))

(define p
  (compose with-guix-guile-instead-of-any-guile
           with-fixed-commonmark))

(define-public gwl/devel
  (package
    (inherit gwl)
    (source #f)
    (arguments
     '(#:make-flags
       '("GUILE_AUTO_COMPILE=0")))
    (inputs
     `(("guix" ,guix)
       ("guile" ,guix-guile)
       ("guile-commonmark" ,(p guile-commonmark))
       ("guile-config" ,(p guile-config))
       ("guile-drmaa" ,(p guile-drmaa))
       ("guile-gcrypt" ,(p guile-gcrypt))
       ("guile-pfds" ,(p guile-pfds))
       ("guile-syntax-highlight" ,(p guile-syntax-highlight))
       ("guile-wisp" ,(p guile-wisp))))
    (native-inputs
     `(("texlive" ,texlive-tiny)       ; for make distcheck
       ("sed" ,sed)

       ;; For "make release"
       ("perl" ,perl)
       ("git" ,git-minimal)

       ;; For manual post processing
       ("guile-lib" ,guile-lib)
       ("rsync" ,rsync)

       ;; For "git push"
       ("ssh" ,openssh)

       ,@(package-native-inputs gwl)))))
