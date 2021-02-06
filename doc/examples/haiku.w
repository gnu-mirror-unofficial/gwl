process haiku
  outputs "haiku.txt"
  synopsis "Write a haiku to a file"
  description
    . "This process writes a haiku by Gary Hotham \
to the file \"haiku.txt\"."
  procedure
    ` with-output-to-file ,outputs
        lambda ()
          display "\
the library book
overdue?
slow falling snow"
