(define-module (extended-example-workflow)
  #:use-module (gwl processes)
  #:use-module (gwl workflows)
  #:use-module (gwl sugar) ; for embedded bash snippet
  #:use-module ((gnu packages compression) #:select (gzip))
  #:use-module ((gnu packages bash) #:select (bash))
  #:use-module (srfi srfi-1)
  #:use-module (example-workflow)) ; We are going to extend "example-workflow".

(define (list-file-template filename)
  (process
   (name (string-append "list-file-" (basename filename)))
   (package-inputs (list bash gzip))
   (data-inputs (list filename))
   (outputs (list (string-append filename ".list")))
   (run-time (complexity
              (space (megabytes 20))
              (time 10)))
   (procedure
# /bin/bash -c
  gzip --list ${_GWL_PROCESS_DATA_INPUTS} > ${_GWL_PROCESS_OUTPUTS}
##
  )))

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
