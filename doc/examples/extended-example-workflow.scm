(import (only (gnu packages compression) gzip)
        (gnu packages bash))

;; We are going to extend "example-workflow".
(define dynamic-workflow
  (load-workflow "example-workflow.scm"))

(define (list-file-template filename)
  (make-process
   (name (string-append "list-file-" (basename filename)))
   (packages (list gzip))
   (inputs (list filename))
   (outputs (list (string-append filename ".list")))
   (run-time (complexity
              (space 20 mebibytes)
              (time 30 seconds)))
   (procedure # { gzip --list {{inputs}} > {{outputs}} })))

;; Get all processes of the other workflow.
(define foreign-processes
  (workflow-processes dynamic-workflow))

;; Get the processes that we want to extend on.
(define compress-file-processes
  (processes-filter-by-name foreign-processes "compress-file"))

;; Create the new processes.
(define list-file-processes
  (map list-file-template
       (append-map process-outputs
                   compress-file-processes)))

(make-workflow
 (name "extended-dynamic-workflow")
 (processes
  (append
   (workflow-restrictions dynamic-workflow)
   (zip list-file-processes compress-file-processes))))
