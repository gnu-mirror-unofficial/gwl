;; We are going to extend the workflow defined in the file
;; "example-workflow.w".
define dynamic-workflow
  load-workflow "example-workflow.w"

process list-file-template (with filename)
  name
    string-append "list-file-"
                  basename filename
  packages "gzip"
  inputs filename
  outputs
    file filename ".list"
  run-time
    complexity
      space 20 mebibytes
      time  30 seconds
  # { gzip --list {{inputs}} > {{outputs}} }


;; Get all processes of the other workflow.
define foreign-processes
  workflow-processes dynamic-workflow

;; Get the processes that we want to extend on.
define compress-file-processes
  processes-filter-by-name foreign-processes "compress-file"

;; Create the new processes.
define list-file-processes
  map list-file-template
      append-map process-outputs compress-file-processes

workflow extended-dynamic-workflow
  processes
    append
      ;; These are the process connections of the imported workflow
      workflow-restrictions dynamic-workflow
      ;; And these are the new process connections.  The "zip" procedure
      ;; pairs up each of the processes in "list-file-processes" with
      ;; one of the processes in "compress-file-processes".
      zip list-file-processes compress-file-processes
