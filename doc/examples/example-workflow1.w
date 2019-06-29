process: create-file
  outputs "/tmp/file.txt"
  run-time
    complexity
      space : megabytes 20
      time  10 ; in seconds
  # { echo hello > {{outputs}} }

process: compress-file
  packages "gzip"
  inputs "/tmp/file.txt"
  outputs "/tmp/file.txt.gz"
  run-time
    complexity
      space : megabytes 20
      time  10
  # { gzip {{inputs}} -c > {{outputs}} }
