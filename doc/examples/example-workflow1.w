process: create-file
  outputs "/tmp/file.txt"
  run-time
    complexity
      space 20 MiB
      time  10 seconds
  # { echo hello > {{outputs}} }

process: compress-file
  packages "gzip"
  inputs "/tmp/file.txt"
  outputs "/tmp/file.txt.gz"
  run-time
    complexity
      space 20 mebibytes
      time   2 minutes
  # { gzip {{inputs}} -c > {{outputs}} }
