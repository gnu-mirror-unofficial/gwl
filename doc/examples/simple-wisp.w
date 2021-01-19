process greet
  packages "hello"
  # { hello }

process sleep
  packages "coreutils"
  # {
    echo "Sleeping..."
    sleep 10
  }

process eat (with something)
  name
    string-append "eat-" something
  # {
    echo "Eating {{something}}"
  }

process bye
  # { echo "Farewell, world!" }

workflow simple-wisp
  processes
    define eat-fruit
      eat "fruit"
    define eat-veges
      eat "vegetables"
    graph
      eat-fruit -> greet
      eat-veges -> greet
      sleep     -> eat-fruit eat-veges
      bye       -> sleep
