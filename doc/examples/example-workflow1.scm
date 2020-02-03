(import (only (gnu packages compression) gzip))

(define-public create-file
  (process
   (name "create-file")
   (outputs (list "/tmp/file.txt"))
   (run-time (complexity
              (space 20 mebibytes)
              (time  10)))
   (procedure
    `(call-with-output-file ,(first outputs)
       (lambda (port)
         (format port "~%"))))))

(define-public compress-file
  (process
   (name "compress-file")
   (packages (list gzip))
   (inputs (list "/tmp/file.txt"))
   (outputs (list "/tmp/file.txt.gz"))
   (run-time (complexity
              (space 20 mebibytes)
              (time  10)))
   (procedure # { gzip {{inputs}} -c > {{outputs}} })))
