process create-file (with filename)
  outputs filename
  run-time
    complexity
      space 20 mebibytes
      time  10 seconds
  # { echo "Hello, world!  This is {{outputs}}." > {{outputs}} }

process compress-file (with input)
  packages "gzip"
  inputs input
  outputs
    file input ".gz"
  run-time
    complexity
      space 20 mebibytes
      time  10 seconds
  # { gzip {{inputs}} -c > {{outputs}} }


;; All inputs files.  The leading dot continues the previous line.
define files
  list "one.txt"
     . "two.txt"
     . "three.txt"

;; Map process templates to files to generate a list of processes.
define create-file-processes
  map create-file files

define compress-file-processes
  map compress-file files

workflow dynamic-workflow
  processes
    auto-connect compress-file-processes create-file-processes
