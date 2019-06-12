(import (only (gnu packages compression) gzip)
        (gnu packages bash)
        (gwl utils)) ; for load-workflow

;; We are going to extend "example-workflow".
(define dynamic-workflow
  (load-workflow "example-workflow.scm"))

(define (list-file-template filename)
  (process
   (name (string-append "list-file-" (basename filename)))
   (packages (list bash gzip))
   (inputs (list filename))
   (outputs (list (string-append filename ".list")))
   (run-time (complexity
              (space (megabytes 20))
              (time 10)))
   (procedure # bash { gzip --list {{inputs}} > {{outputs}} })))

(define-public extended-dynamic-workflow
  (let* (;; Get all processes of the other workflow.
         (foreign-processes (workflow-processes dynamic-workflow))

         ;; Get the processes that we want to extend on.
         (compress-file-processes (processes-filter-by-name
                                   foreign-processes "compress-file"))

         ;; Create the new processes.
         (list-file-processes (map list-file-template
                                   (map (compose first process-outputs)
                                        compress-file-processes))))
    (workflow
     (name "extended-dynamic-workflow")
     (processes
      (append
       (workflow-restrictions dynamic-workflow)
       (zip list-file-processes compress-file-processes))))))

extended-dynamic-workflow
