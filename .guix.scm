(use-modules (guix build-system gnu)
             (guix packages)
             ((guix licenses) #:prefix license:)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages package-management)
             (gnu packages pkg-config))

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
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.2)))
    (propagated-inputs
     `(("guix" ,guix)
       ("guile-commonmark" ,guile-commonmark)
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
