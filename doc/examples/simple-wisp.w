define-module : simple-wisp

import
  gwl processes
  gwl workflows
  gwl sugar

process: greet
  package-inputs
    list "hello"
  procedure '(system "hello")

process: sleep
  package-inputs : list "coreutils"
  procedure '(begin (display "Sleeping...\n")
                    (system* "sleep" "10"))

process: (eat something)
  name
    string-append "eat-" something
  procedure `(format #t "Eating ~a\n" ,something)

process: bye
  procedure '(display "Farewell, world!\n")

workflow: simple-wisp
  processes
    let
      :
        eat-fruit : eat "fruit"
        eat-veges : eat "vegetables"
      graph
        eat-fruit -> greet
        eat-veges -> greet
        sleep     -> eat-fruit eat-veges
        bye       -> sleep
