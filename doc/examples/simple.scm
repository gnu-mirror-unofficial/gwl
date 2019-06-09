(import (gnu packages base))

(define-public greet
  (process
   (name "greet")
   (package-inputs (list hello))
   (procedure '(system "hello"))))

(define-public sleep
  (process
   (name "sleep")
   (package-inputs (list coreutils))
    (procedure
     '(begin
        (display "Sleeping...\n")
        (system "sleep 10")))))

(define-public (eat something)
  (process
   (name (string-append "eat-" something))
   (procedure
    `(format #t "Eating ~a\n" ,something))))

(define-public bye
  (process
   (name "bye")
   (procedure
    '(display "Farewell, world!\n"))))

(define-public simple
  (workflow
   (name "simple")
   (processes
    (let ((eat-fruit (eat "fruit"))
          (eat-veges (eat "vegetables")))
      (graph (eat-fruit -> greet)
             (eat-veges -> greet)
             (sleep     -> eat-fruit eat-veges)
             (bye       -> sleep))))))
