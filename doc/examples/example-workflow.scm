(define-module (example-workflow)
  #:use-module (gwl processes)
  #:use-module (gwl workflows)
  #:use-module (gwl sugar) ; for inline bash snippet
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (srfi srfi-1)) ; For "first" and "append"

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

(define-public dynamic-workflow
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
