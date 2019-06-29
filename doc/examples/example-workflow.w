process: (create-file filename)
  name
    string-append "create-file-"
                  basename filename
  outputs filename
  run-time
    complexity
      space : megabytes 20
      time    10
  # { echo "Hello, world!" > {{filename}} }

process: (compress-file input)
  name
    string-append "compress-file-"
                  basename input
  packages "gzip"
  inputs input
  outputs
    string-append input ".gz"
  run-time
    complexity
      space : megabytes 20
      time    10
  # { gzip {{inputs}} -c > {{outputs}} }


;; All inputs files.  The leading dot continues the previous line.
define files
  list "/tmp/one.txt"
     . "/tmp/two.txt"
     . "/tmp/three.txt"

;; Map process templates to files to generate a list of processes.
define create-file-processes
  map create-file files

define compress-file-processes
  map compress-file files

workflow: dynamic-workflow
  processes
    apply auto-connect
          append compress-file-processes create-file-processes
