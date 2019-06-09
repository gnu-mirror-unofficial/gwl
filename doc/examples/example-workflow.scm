(import (gnu packages bash)
        (gnu packages compression))

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

(define (compress-file input)
  (process
   (name (string-append "compress-file-" (basename input)))
   (package-inputs (list gzip bash))
   (data-inputs (list input))
   (outputs (list (string-append input ".gz")))
   (run-time (complexity
              (space   (megabytes 20))
              (time    10)))
   (procedure # bash { gzip {{data-inputs}} -c > {{outputs}} })))

(define dynamic-workflow
  (workflow
   (name "dynamic-workflow")
   (processes
    (let* ((files '("/tmp/one.txt"
                    "/tmp/two.txt"
                    "/tmp/three.txt"))
           (create-file-processes
            (map create-file files))
           (compress-file-processes
            (map compress-file files)))
      (apply auto-connect
             (append compress-file-processes
                     create-file-processes))))))

dynamic-workflow
