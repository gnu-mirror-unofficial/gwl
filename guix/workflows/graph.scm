(define-module (guix workflows graph)
  #:use-module (ice-9 format)
  #:use-module (guix workflows)
  #:use-module (guix processes)
  #:export (workflow->dot))

;;; ---------------------------------------------------------------------------
;;; GRAPHING FUNCTIONALITY
;;; ---------------------------------------------------------------------------

(define take-color (color-scheme-stepper %modern-color-scheme))

(define (workflow-dot-prettify-node process)
  "Returns a string of prettified node names for a Graphviz graph."
  (let* ((proc process)
         (pretty-name (string-map (lambda (x)
                                    (if (eq? x #\-) #\  x))
                                  (process-name proc))))
    (format #f (string-append " ~s [shape=box,style=\"rounded,filled\",fillcolo"
                              "r=~s,label=<<FONT POINT-SIZE=\"14\"><U>~a</U></F"
                              "ONT><BR/><FONT POINT-SIZE=\"12\">~a<BR/><BR/>Use"
                              "s: ~{~a~^, ~}.</FONT>>];~%")
            (process-full-name proc)
            (take-color)
            (string-upcase pretty-name)
            (process-synopsis proc)
            (if (process-package-inputs proc)
                (map (lambda (pair)
                       (package-full-name (cadr pair)))
                     (process-package-inputs proc))
                '("-")))))

(define (workflow-restriction->dot pair)
  "Write the dependency relationships of a restriction in dot format."
  (let ((process (process-full-name (car pair)))
        (restrictions (cdr pair)))
    (format #f "~{~a~}~%" (map (lambda (item)
                                 (format #f "~s -> ~s~%"
                                         (process-full-name item)
                                         process))
                               restrictions))))

(define* (workflow->dot workflow #:key (parallel? #t))
  "Returns the workflow's processes formatted in Graphviz's Dot language as a
directed acyclic graph."
  (format #f "digraph G {~%  graph [bgcolor=transparent, fontsize=24];~%~{~a~}~%~{~a~}}"
          (map workflow-dot-prettify-node (workflow-processes workflow))
          (map workflow-restriction->dot (workflow-restrictions workflow))))
