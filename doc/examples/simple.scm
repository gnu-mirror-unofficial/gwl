(import (gnu packages base))

(define-public greet
  (make-process
   (name "greet")
   (packages (list hello))
   (procedure '(system "hello"))))

(define-public sleep
  (make-process
   (name "sleep")
   (packages (list coreutils))
    (procedure
     '(begin
        (display "Sleeping...\n")
        (system "sleep 10")))))

(define-public (eat something)
  (make-process
   (name (string-append "eat-" something))
   (procedure
    `(format #t "Eating ~a\n" ,something))))

(define-public bye
  (make-process
   (name "bye")
   (procedure
    '(display "Farewell, world!\n"))))

(make-workflow
 (name "simple")
 (processes
  (let ((eat-fruit (eat "fruit"))
        (eat-veges (eat "vegetables")))
    (graph (eat-fruit -> greet)
           (eat-veges -> greet)
           (sleep     -> eat-fruit eat-veges)
           (bye       -> sleep)))))
