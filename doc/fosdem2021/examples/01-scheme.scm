(define train
  (make-process
   (name "train")
   (packages
    (list "hello"
          "automake"
          "perl"))
   (description
    "Train AI for 5G augmented reality blockchain.")
   (procedure
    (display "Choo choo!"))))
