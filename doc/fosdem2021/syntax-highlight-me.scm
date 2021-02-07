(import (gwl www util)
        (sxml xpath)
        (sxml simple)
        (sxml transform)
        (pict)
        (ice-9 textual-ports)
        (ice-9 match)
        (syntax-highlight)
        (syntax-highlight scheme)
        (srfi srfi-1))

(define font-size 28)

(define (stylesheet class)
  (case class
    ((*default*)
     `(#:font-family "Fira Code"
       #:font-size ,font-size))
    ((function)
     '(#:color "#586e75"
       #:font-style "italic" ))
    ((placeholder)
     '(#:color "#268bd2"))
    ((open)
     '(#:color "#657b83"))
    ((close)
     '(#:color "#657b83"))
    ((string)
     '(#:color "#dc322f"))
    ((comment)
     '(#:color "#93a1a1"))
    ((special)
     '(#:color "#002b36"
       #:font-weight bold))
    ((symbol)
     '(#:color "#657b83"))
    ((gwl-field)
     '(#:color "#859900"
       #:font-weight bold))
    (else '())))

(define* (code->svg code
                    #:key
                    (margin-x 0) (margin-y 0)
                    (font-size 12)
                    (stylesheet (const '()))
                    lexer)
  (define (render-element thing)
    (match thing
      ((tag txt)
       (apply text (cons txt
                         (append (stylesheet '*default*)
                                 (stylesheet tag)))))
      ((? string? txt)
       (call-with-values
           (lambda () (span (lambda (char)
                         (eq? char #\space))
                       (string->list txt)))
         (lambda (indent after)
           (hc-append
            (ghost (rectangle (* 0.8 ; XXX
                                 (length indent)
                                 (pict-width
                                  (apply text
                                         (cons "."
                                               (stylesheet '*default*)))))
                              font-size
                              #:border-width 0))
            (apply text
                   (cons (list->string after)
                         (stylesheet '*default*)))))))))
  (define (render-line line)
    (if (string-null? line)
        (ghost (rectangle 1 font-size))
        (apply hc-append
               (map render-element
                    (highlight lexer line)))))
  (define rendered-text
    (let ((lines (string-split code #\newline)))
      (apply vl-append (map render-line lines))))
  (cc-superimpose (filled-rectangle (+ (pict-width rendered-text)
                                       margin-x)
                                    (+ (pict-height rendered-text)
                                       margin-y)
                                    #:color "#eee8d5")
                  rendered-text))

(define* (make-slide body #:key (width 1280) (height 720))
  (let ((background (filled-rectangle width height #:color "#eee8d5")))
    (lt-superimpose background
                    (hc-append (ghost (rectangle 200 height))
                               body))))

(define (main . args)
  (let ((file (cadr (command-line))))
    (display file)
    (pict->file
     (make-slide (code->svg (call-with-input-file file get-string-all)
                            #:margin-x 30 #:margin-y 30 #:font-size font-size
                            #:stylesheet stylesheet
                            #:lexer (if (string-suffix? file ".scm")
                                        lex-scheme
                                        lex-gwl)))
     (string-append file ".svg"))))
