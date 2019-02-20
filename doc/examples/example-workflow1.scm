(define-module (example-workflow1)
  #:use-module (gwl processes)
  #:use-module (gwl workflows)
  #:use-module (gwl sugar) ; for inline bash snippet
  #:use-module ((gnu packages bash) #:select (bash))
  #:use-module ((gnu packages compression) #:select (gzip))
  #:use-module (srfi srfi-1))

(define-public create-file
  (process
   (name "create-file")
   (outputs (list "/tmp/file.txt"))
   (run-time (complexity
              (space (megabytes 20))
              (time  10)))
   (procedure
    `(call-with-output-file ,(first outputs)
       (lambda (port)
         (format port "~%"))))))

(define-public compress-file
  (process
   (name "compress-file")
   (package-inputs (list gzip bash))
   (data-inputs (list "/tmp/file.txt"))
   (outputs (list "/tmp/file.txt.gz"))
   (run-time (complexity
              (space (megabytes 20))
              (time  10)))
   (procedure # bash { gzip {{data-inputs}} -c > {{outputs}} })))
