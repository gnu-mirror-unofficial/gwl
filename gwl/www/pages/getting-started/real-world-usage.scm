;;; Copyright © 2017  Roel Janssen <roel@gnu.org>
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

(define-module (gwl www pages getting-started real-world-usage)
  #:use-module (gwl www pages)
  #:export (page-getting-started-real-world-usage))

(define (page-getting-started-real-world-usage request-path)
  (page-root-template
   "Real-world usage with GWL" request-path
   `((h2 "Real-world usage with GWL")
     (p "In the previous parts of this tutorial we looked at what a "
        (code "process") " and a "  (code "workflow") " look like in the GWL. "
        "In this section we discuss how to use the GWL in real-world "
        "situations.")

     (p "Every programming language has its powerful applications.  If you're "
        "doing statistical analysis, you may very well be using R.  For "
        "critical performance applications, you are probably using C, C++ or D."
        "  But for deploying and running these programs and scripts, you can "
        "use Scheme, with GNU Guix and GWL.")

     (h3 "Automatic deployment of your programs and scripts")

     (p "By using GNU Guix's " (code "package") " descriptions, we include the "
        "computational description of deploying a program into the workflow by "
        "refering to the Scheme variable that represents the " (code "package")
        ".")

     (p "We can follow the same approach for our own scripts.  The first step "
        "is to bundle your scripts in a tarball (web interfaces for Git like "
        "Github and Gitlab do this automatically if you tag a commit).  The "
        "rest of the package description can be found in " (em "Figure 9") ".")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                      "(define-public my-pipeline-scripts
  (package
    (name \"my-pipeline-scripts\")
    (version \"1.0\")
    (source (origin
              (method url-fetch)
              (uri \"https://github.com/example/my-scripts/archive/v0.1.tar.gz\")
              (sha256
               (base32
                \"...\"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs \"out\"))
                (script-dir (string-append out \"/share/my-scripts/\"))
                (tar  (string-append (assoc-ref %build-inputs \"tar\") \"/bin/tar\"))
                (PATH (string-append (assoc-ref %build-inputs \"gzip\") \"/bin\")))
           (mkdir-p script-dir)
           (setenv \"PATH\" PATH)
           (system* tar \"xvf\" (assoc-ref %build-inputs \"source\"))
           (chdir \"my-scripts\")
           (install-file \"first-script.R\" script-dir)
           (install-file \"second-script.py\" script-dir)
           (install-file \"third-script.pl\" script-dir)))))
    (native-inputs
     `((\"gzip\" ,gzip)
       (\"tar\" ,tar)))
    (propagated-inputs
     `((\"r-genomicranges\" ,r-genomicranges)
       (\"python2-pandas\" ,python2-pandas)))
    (home-page #f)
    (synopsis \"Additional scripts for my pipeline.\")
    (description \"Additional scripts for my pipeline.\")
(license #f)))"))
          (p (strong "Figure 9") ": Scheme code to package your own scripts."))

     (h3 "Using software packages in your process descriptions")

     (p "To express that before running a process, our programs must be made "
        "available, we use the " (code "package-inputs") " field.  "
        (em "Figure 10") " contains an example of how to use this field.")

     (p "In addition to software tools, we need to describe where to find our "
        "data as well.  Similarly to " (code "package-inputs") ", we can use "
        "the field " (code "data-inputs") " for this purpose.")

     (p "The point of a workflow is to make the " (code "data-inputs")
        " variable.  To describe the entire “deploy and run " (em "this")
        " program, on " (em "this") " input file”, where the input file "
        "can be variable, we can define a function as in " (em "Figure 10")
        ".")

     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                      "(define-public (samtools-idxstats-for-samples samples data-path)
  (process
   (name \"samtools-idxstats\")
   (version \"1.0\")
   (package-inputs
    `((\"samtools\" ,samtools)))
   (data-inputs
    `((\"samples\" . ,samples)))
   (run-time (complexity
              (space (megabytes 250))
              (time (minutes 20))))
   (procedure
    #~(let ((samtools (string-append #$@(assoc-ref package-inputs \"samtools\")
                                     \"/bin/samtools\")))
        (zero? (apply +
                 (map (lambda (sample)
                        (system
                          (string-append
                           samtools \" idxstats \"
                           #$data-path \"/\" sample \"/mapping/\" sample \"_dedup.bam \"
                           \"> \" sample \".idxstats.txt\")))
                      '#$(assoc-ref data-inputs \"samples\"))))))
   (synopsis \"Retrieve statistics from a BAM index file\")
(description \"This process retrieves statistics from a BAM index file.\")))"))
          (p (strong "Figure 10") ": Specify and use packages in a process."))

     (p "From which we can get processes using: ")
     (div (@ (class "figure"))
          (pre (code (@ (class "scheme"))
                      "(define-public actual-proces (samtools-idx-for-samples "
                      "'(\"sample1\" \"sample2\" \"sample3\") \"/base/path/to/"
                      "samples\")"))
          (p (strong "Figure 11") ": Derive a process from the function defined "
             "in " (em "Figure 10") "."))

     (h3 "A little implementation detail")

     (p "For the careful readers, there is a slight difference between "
        (code "(key . value)") " and " (code "(key value)") " in Scheme.  In "
        "a G-Expression, we can get " (code "value") " from the first form by "
        "using " (code "#$(assoc-ref pair \"key\")") ", and for the second "
        "form, we have to use " (code "#$@(assoc-ref pair \"key\")") "."))
   #:dependencies '(highlight)))

