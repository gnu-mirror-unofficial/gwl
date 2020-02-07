;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2012-2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gwl utils)
  #:use-module (gwl ui)
  #:use-module (gwl processes)
  #:use-module (gwl workflows)
  #:use-module (pfds sets)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-31)
  #:use-module (srfi srfi-34)
  #:use-module (language wisp)
  #:export (load-workflow
            wisp-suffix
            on))

;; Convenience procedure to simplify Wisp syntax of higher-order
;; procedures such as "map" by having the collection first.
(define (on collection higher-proc item-proc)
  (higher-proc item-proc collection))

(define (wisp-suffix file)
  (cond ((string-suffix? ".w" file) ".w")
        ((string-suffix? ".wisp" file) ".wisp")
        ((string-suffix? ".gwl" file) ".gwl")
        (else #f)))

;; Taken from (guix ui).
(define (make-user-module modules)
  "Return a new user module with the additional MODULES loaded."
  ;; Module in which the machine description file is loaded.
  (let ((module (make-fresh-user-module)))
    (for-each (lambda (iface)
                (module-use! module (resolve-interface iface)))
              modules)
    module))

(define (load-workflow* file)
  "Load the workflow specified in FILE in the context of a new module
where all the basic GWL modules are available."
  (define modules
    (if (wisp-suffix file)
        '((gwl processes)
          (gwl workflows)
          (gwl sugar)
          (gwl utils)
          (srfi srfi-1)
          (srfi srfi-88))
        '((gwl processes)
          (gwl workflows)
          (gwl sugar reader)
          (gwl utils)
          (srfi srfi-1))))
  (let ((result (load* file (make-user-module modules))))
    (unless (workflow? result)
      (format (current-error-port)
              "File `~a' does not evaluate to a workflow value.~%"
              file)
      (exit 1))
    result))

;; Helper to handle relative file names.
(define-syntax-rule (load-workflow file)
  (let ((target (string-append (dirname (or (current-filename)
                                            (*current-filename*) ""))
                               "/" file)))
    (load-workflow*
     (if (or (absolute-file-name? file)
             (not (file-exists? target)))
         file target))))



(define-record-type <location>
  (make-location file line column)
  location?
  (file          location-file)         ; file name
  (line          location-line)         ; 1-indexed line
  (column        location-column))      ; 0-indexed column

(define (location file line column)
  "Return the <location> object for the given FILE, LINE, and COLUMN."
  (and line column file
       (make-location file line column)))

(define (source-properties->location loc)
  "Return a location object based on the info in LOC, an alist as returned
by Guile's `source-properties', `frame-source', `current-source-location',
etc."
  ;; In accordance with the GCS, start line and column numbers at 1.  Note
  ;; that unlike LINE and `port-column', COL is actually 1-indexed here...
  (match loc
    ((('line . line) ('column . col) ('filename . file)) ;common case
     (and file line col
          (make-location file (+ line 1) col)))
    (#f
     #f)
    (_
     (let ((file (assq-ref loc 'filename))
           (line (assq-ref loc 'line))
           (col  (assq-ref loc 'column)))
       (location file (and line (+ line 1)) col)))))

(define (location->source-properties loc)
  "Return the source property association list based on the info in LOC,
a location object."
  `((line     . ,(and=> (location-line loc) 1-))
    (column   . ,(location-column loc))
    (filename . ,(location-file loc))))

(define (location->string loc)
  "Return a human-friendly, GNU-standard representation of LOC."
  (match loc
    (#f (G_ "<unknown location>"))
    (($ <location> file line column)
     (format #f "~a:~a:~a" file line column))))

;; TODO: prettify, add colors, etc
(define (report-error . args)
  (match args
    (((? location? loc) . rest)
     (format (current-error-port)
             "~a: ~a" (location->string loc)
             (apply format #f rest)))
    (_
     (apply format (current-error-port) args))))
(define (display-hint . args)
  (apply format (current-error-port) args))

;; Taken from (guix ui).
(define (known-variable-definition variable)
  "Search among the currently loaded modules one that defines a variable named
VARIABLE and return it, or #f if none was found."
  (define (module<? m1 m2)
    (match (module-name m2)
      (('gwl _ ...) #t)
      (_ #f)))

  (let loop ((modules     (list (resolve-module '() #f #f #:ensure #f)))
             (suggestions '())
             (visited     (make-set <)))
    (match modules
      (()
       ;; Pick the "best" suggestion.
       (match (sort suggestions module<?)
         (() #f)
         ((first _ ...) first)))
      ((head tail ...)
       (if (set-member? visited head)
           (loop tail suggestions visited)
           (let ((visited (set-insert head visited))
                 (next    (append tail
                                  (hash-map->list (lambda (name module)
                                                    module)
                                                  (module-submodules head)))))
             (match (module-local-variable head variable)
               (#f (loop next suggestions visited))
               (_
                (match (module-name head)
                  (('gwl _ ...) head)   ;must be that one
                  (_ (loop next (cons head suggestions) visited)))))))))))

(define* (report-unbound-variable-error args #:key frame)
  "Return the given unbound-variable error, where ARGS is the list of 'throw'
arguments."
  (match args
    ((key . args)
     (print-exception (current-error-port) frame key args)))
  (match args
    (('unbound-variable proc message (variable) _ ...)
     (match (known-variable-definition variable)
       (#f
        (display-hint (G_ "Did you forget a @code{use-modules} form?")))
       ((? module? module)
        (display-hint (format #f (G_ "Did you forget @code{(use-modules ~a)}?")
                              (module-name module))))))))

;; Adapted from (guix ui).
(define* (report-load-error file args #:optional frame)
  "Report the failure to load FILE, a user-provided Scheme or Wisp file.
ARGS is the list of arguments received by the 'throw' handler."
  (match args
    (('system-error . rest)
     (let ((err (system-error-errno args)))
       (report-error (G_ "failed to load '~a': ~a~%") file (strerror err))))
    (('read-error "scm_i_lreadparen" message _ ...)
     ;; Guile's missing-paren messages are obscure so we make them more
     ;; intelligible here.
     (if (string-suffix? "end of file" message)
         (let ((location (string-drop-right message
                                            (string-length "end of file"))))
           (format (current-error-port) (G_ "~amissing closing parenthesis~%")
                   location))
         (apply throw args)))
    (('read-error "scm_read_extended_symbol" message _ ...)
     ;; This error indicates that "#{" was used instead of "# {"
     (if (string-suffix? "end of file while reading symbol" message)
         (let ((location (string-drop-right message
                                            (string-length "end of file while reading symbol"))))
           (format (current-error-port) (G_ "~aUnterminated extended symbol. Did you mean to use \"# {\" instead of \"#{\"?~%")
                   location))
         (apply throw args)))
    (('syntax-error proc message properties form . rest)
     (let ((loc (source-properties->location properties)))
       (report-error loc (G_ "~a~%") message)))
    (('unbound-variable _ ...)
     (report-unbound-variable-error args #:frame frame))
    ((key args ...)
     (report-error (G_ "failed to load '~a':~%") file)
     (match args
       (((? symbol? proc) (? string? message) (args ...) . rest)
        (display-error frame (current-error-port) proc message
                       args rest))
       (_
        ;; Some exceptions like 'git-error' do not follow Guile's convention
        ;; above and need to be printed with 'print-exception'.
        (print-exception (current-error-port) frame key args))))))

(define (last-frame-with-source stack)
  "Walk stack upwards and return the last frame that has source location
information, or #f if it could not be found."
  (define (frame-with-source frame)
    ;; Walk from FRAME upwards until source location information is found.
    (let loop ((frame    frame)
               (previous frame))
      (if (not frame)
          previous
          (if (frame-source frame)
              frame
              (loop (frame-previous frame) frame)))))

  (and stack
       (let* ((depth (stack-length stack))
              (last  (and (> depth 0) (stack-ref stack 0))))
         (frame-with-source (if (> depth 1)
                                (stack-ref stack 1)    ;skip the 'throw' frame
                                last)))))


(define (read-one-wisp-sexp port)
  "Read a Wisp expression from PORT."
  ;; allow using "# foo" as #(foo).
  (read-hash-extend #\# (λ (chr port) #\#))
  (cond
   ((eof-object? (peek-char port))
    (read-char port)) ; return eof: we’re done
   (else
    (match (wisp-scheme-read-chunk port)
      (() #f)
      ((chunk . _) chunk)))))

;; Adapted from (guix ui).
(define* (load* file user-module)
  "Load the user provided Scheme or Wisp source code FILE."
  (define tag
    (make-prompt-tag "user-code"))

  (catch #t
    (lambda ()

      ;; Force re-compilation to avoid ABI issues
      (set! %fresh-auto-compile #t)
      (set! %load-should-auto-compile #t)

      (save-module-excursion
       (lambda ()
         (set-current-module user-module)

         ;; Hide the "auto-compiling" messages.
         (parameterize ((current-warning-port (%make-void-port "w")))
           (call-with-prompt tag
             (lambda ()
               ;; XXX: The Wisp reader fails to set source properties in all
               ;; cases, so (current-filename) always returns #F.
               (module-define! user-module '*current-filename*
                               (make-parameter file))
               ;; Give 'load' an absolute file name so that it doesn't
               ;; try to search for FILE in %LOAD-COMPILED-PATH.
               (load (canonicalize-path file)
                     (and (wisp-suffix file)
                          read-one-wisp-sexp)))
             (const #f))))))
    (lambda _
      (exit 1))
    (rec (handle-error . args)
         ;; Capture the stack up to this procedure call, excluded, and pass
         ;; the faulty stack frame to 'report-load-error'.
         (let* ((stack (make-stack #t handle-error tag))
                (frame (last-frame-with-source stack)))
           (report-load-error file args frame)))))
