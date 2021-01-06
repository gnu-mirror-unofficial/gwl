;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This file is part of the Guix Workflow Language.
;;; Parts of it were taken from Guix.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (htmlprag)
             (gwl www util)
             ((gwl workflows utils) #:select (mkdir-p))
             (syntax-highlight)
             (syntax-highlight scheme)
             (syntax-highlight lexers)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match)
             (ice-9 ftw))

(define (syntax-highlighted-html %input %output)
  "Process all the HTML files in INPUT by highlighting the syntax of all its
<pre class=\"lisp\"> blocks (as produced by 'makeinfo --html').  Write
generated files to %output."
  (begin
    (define entity->string
      (match-lambda
        ("rArr"   "⇒")
        ("rarr"   "→")
        ("hellip" "…")
        ("rsquo"  "’")
        (e (pk 'unknown-entity e) (primitive-exit 2))))

    (define (concatenate-snippets pieces)
      ;; Concatenate PIECES, which contains strings and entities,
      ;; replacing entities with their corresponding string.
      (let loop ((pieces pieces)
                 (strings '()))
        (match pieces
          (()
           (string-concatenate-reverse strings))
          (((? string? str) . rest)
           (loop rest (cons str strings)))
          ((('*ENTITY* "additional" entity) . rest)
           (loop rest (cons (entity->string entity) strings)))
          ((('span _ lst ...) . rest)   ;for <span class="roman">
           (loop (append lst rest) strings))
          ((('var name) . rest)             ;for @var{name} within @lisp
           (loop rest (cons name strings))) ;XXX: losing formatting
          (something
           (pk 'unsupported-code-snippet something)
           (primitive-exit 1)))))

    (define (highlight-definition id category symbol args)
      ;; Produce stylable HTML for the given definition (an @deftp,
      ;; @deffn, or similar).
      `(dt (@ (id ,id) (class "symbol-definition"))
           (span (@ (class "symbol-definition-category"))
                 ,@category)
           (span (@ (class "symbol-definition-prototype"))
                 ,symbol " " ,@args)))

    (define (space? obj)
      (and (string? obj)
           (string-every char-set:whitespace obj)))

    (define (syntax-highlight sxml)
      ;; Recurse over SXML and syntax-highlight code snippets.
      (let loop ((sxml sxml))
        (match sxml
          (('*TOP* decl body ...)
           `(*TOP* ,decl ,@(map loop body)))
          ((or ('div ('@ ('class "lisp"))
                     (? space?)      ; annoying!
                     ('pre ('@ ('class (or "verbatim" "lisp"))) code-snippet ...))
               ('pre ('@ ('class "lisp")) code-snippet ...))
           (let* ((code (concatenate-snippets code-snippet))
                  (scheme? (string-prefix? "(" code)))
             `(pre (@ (class "lisp"))
                   (code (@ (class ,(if scheme? "scheme" "gwl")))
                         ,@(highlights->sxml
                            (highlight (if scheme?
                                           lex-scheme
                                           lex-gwl) code))))))

          ;; Replace the ugly <strong> used for @deffn etc., which
          ;; translate to <dt>, with more stylable markup.
          (('dt (@ ('id id)) category ... ('strong thing))
           (highlight-definition id category thing '()))
          (('dt (@ ('id id)) category ... ('strong thing)
                (? space?) ('em args ...))
           (highlight-definition id category thing args))

          ((tag ('@ attributes ...) body ...)
           `(,tag (@ ,@attributes) ,@(map loop body)))
          ((tag body ...)
           `(,tag ,@(map loop body)))
          ((? string? str)
           str))))

    (define (process-html file)
      ;; Parse FILE and perform syntax highlighting for its Scheme
      ;; snippets.  Install the result to %output.
      (format (current-error-port) "processing ~a...~%" file)
      (let* ((shtml        (call-with-input-file file html->shtml))
             (highlighted  (syntax-highlight shtml))
             (base         (string-drop file (string-length %input)))
             (target       (string-append %output base)))
        (mkdir-p (dirname target))
        (call-with-output-file target
          (lambda (port)
            (write-shtml-as-html highlighted port)))))

    (define (html? file)
      (string-suffix? ".html" file))

    (ftw %input (lambda (filename statinfo flag)
                  (match flag
                    ('regular
                     (and (html? filename)
                         (process-html filename)))
                    (_ #t))
                  #t))))

(define (main args)
  (setlocale LC_ALL "en_US.utf8")
  (apply syntax-highlighted-html (cdr args)))
