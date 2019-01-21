(define-module (example-workflow)
  #:use-module (gwl processes)
  #:use-module (gwl workflows)
  ;; "zip" is both a package name and a function.  So we use a prefix
  ;; for packages to avoid this collision.
  #:use-module ((gnu packages compression) #:prefix package:)
  #:use-module (srfi srfi-1)) ; For "first", "append", and "zip"

(define (create-file filename)
  (process
   (name (string-append "create-file-" (basename filename)))
   (outputs (list filename))
   (run-time (complexity
              (space   (megabytes 20))
              (time    10)))
   (procedure
    `(call-with-output-file ,(first outputs)
       (lambda (port)
         (format port "Hello, world!~%"))))))

(define (compress-file input output)
  (process
   (name (string-append "compress-file-" (basename input)))
   (package-inputs (list package:gzip))
   (data-inputs (list input))
   (outputs (list output))
   (run-time (complexity
              (space   (megabytes 20))
              (time    10)))
   (procedure
    `(system ,(string-append "gzip "
                             (first data-inputs)
                             " -c > "
                             (first outputs))))))

(define-public dynamic-workflow
  (let* ((files '("/tmp/one.txt" "/tmp/two.txt" "/tmp/three.txt"))
         (create-file-processes   (map create-file files))
         (compress-file-processes (map (lambda (filename)
                                         (compress-file filename (string-append filename ".gz")))
                                       files)))
    (workflow
     (name "dynamic-workflow")
     (processes
      (apply connect
             (append compress-file-processes create-file-processes))))))
