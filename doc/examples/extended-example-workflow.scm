(define-module (extended-example-workflow)
  #:use-module (gwl processes)
  #:use-module (gwl workflows)
  #:use-module ((gnu packages compression) #:prefix package:)
  #:use-module (srfi srfi-1)
  #:use-module (example-workflow)) ; We are going to extend "example-workflow".

(define (delete-file-template filename)
  (process
   (name (string-append "delete-file-" (basename filename)))
   (run-time (complexity
              (space (megabytes 20))
              (time 10)))
   (procedure
    `(delete-file ,filename))))

(define-public extended-dynamic-workflow
  (let* (;; Get all processes of the other workflow.
         (foreign-processes (workflow-processes dynamic-workflow))

         ;; Get the processes that we want to extend on.
         (compress-file-processes (processes-filter-by-name
                                   foreign-processes "compress-file"))

         ;; Create the new processes.
         (delete-file-processes (map delete-file-template
                                     (map (compose first process-outputs)
                                          compress-file-processes))))
    (workflow
     (name "extended-dynamic-workflow")
     (processes
      (append
       (workflow-restrictions dynamic-workflow)
       (zip delete-file-processes compress-file-processes))))))
