;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl errors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (pfds sets)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix colors)
  #:use-module (gwl ui)
  #:use-module ((oop goops) #:select (class-name class-of))
  #:export (&formatted-message
            formatted-message-string
            formatted-message-arguments

            &gwl-error
            gwl-error?

            &gwl-package-error
            gwl-package-error-package-spec

            &gwl-type-error
            gwl-type-error-expected-type
            gwl-type-error-actual-value

            last-frame-with-source
            report-load-error
            report-error
            leave))


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
    ;; This is the format returned by `frame-source'
    (((? number? pc) (? string? file) (? number? line) . (? number? col))
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


(define-condition-type &formatted-message &error
  formatted-message?
  (format    formatted-message-string)
  (arguments formatted-message-arguments))

(define-condition-type &error-location &error
  error-location?
  (location  error-location))                     ;<location>

(define-condition-type &fix-hint &condition
  fix-hint?
  (hint condition-fix-hint))                      ;string

(define-condition-type &gwl-error &error
  gwl-error?)

(define-condition-type &gwl-type-error &gwl-error
  gwl-type-error?
  (expected-type gwl-type-error-expected-type)
  (actual-value gwl-type-error-actual-value))

(define-condition-type &gwl-syntax-error &gwl-error
  gwl-syntax-error?)

(define-condition-type &gwl-package-error &gwl-error
  gwl-package-error?
  (package-spec gwl-package-error-package-spec))

(define* (print-diagnostic-prefix prefix #:optional location
                                  #:key (colors (color)))
  "Print PREFIX as a diagnostic line prefix."
  (define color?
    (color-output? (current-error-port)))

  (define location-color
    (if color?
        (cut colorize-string <> (color BOLD))
        identity))

  (define prefix-color
    (if color?
        (lambda (prefix)
          (colorize-string prefix colors))
        identity))

  (if (location? location)
      (format (current-error-port) "~a: ~a"
              (location-color (location->string location))
              (prefix-color prefix))
      (format (current-error-port) "~a"
              (prefix-color prefix))))

(define %hint-color (color BOLD CYAN))
(define %error-color (color BOLD RED))

(define (report-error location fmt . args)
  (print-diagnostic-prefix (G_ "error: ") location #:colors %error-color)
  (apply format (current-error-port) fmt args))

(define (leave . args)
  (match args
    (((? string? first) . rest)
     (apply format (current-error-port) args))
    (_
     (apply report-error args)))
  (exit 1))

(define* (display-hint message #:optional (port (current-error-port)))
  "Display MESSAGE, a l10n message, to PORT."
  (define colorize
    (if (color-output? port)
        (lambda (str)
          (colorize-string str %hint-color))
        identity))

  (display (colorize (G_ "hint: ")) port)
  (display message port))

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
        (display-hint (G_ "Did you forget an @code{import} form?")))
       ((? module? module)
        (display-hint (format #f (G_ "Did you forget @code{(import ~a)}?")
                              (module-name module))))))))

;; Adapted from (guix ui).
(define* (report-load-error file args #:optional frame)
  "Report the failure to load FILE, a user-provided Scheme or Wisp file.
ARGS is the list of arguments received by the 'throw' handler."
  (define loc
    (and frame (source-properties->location (frame-source frame))))
  (match args
    (('system-error "open-file" format-string . strings)
     (report-error loc (G_ "failed to load '~a': ~a~%") file
                   (apply format #false "~?" format-string strings)))
    (('system-error . rest)
     (let ((err (system-error-errno args)))
       (report-error loc (G_ "failed to load '~a': ~a~%") file (strerror err))))
    (('read-error "scm_i_lreadparen" message _ ...)
     ;; Guile's missing-paren messages are obscure so we make them more
     ;; intelligible here.
     (if (string-suffix? "end of file" message)
         (let ((location (string-drop-right message
                                            (string-length "end of file"))))
           (print-diagnostic-prefix (G_ "error: ") #:colors %error-color)
           (format (current-error-port) (G_ "~amissing closing parenthesis~%")
                   location))
         (apply throw args)))
    (('read-error "scm_read_extended_symbol" message _ ...)
     (print-diagnostic-prefix (G_ "error: ") #:colors %error-color)
     ;; This error indicates that "#{" was used instead of "# {"
     (if (string-suffix? "end of file while reading symbol" message)
         (let ((location (string-drop-right message
                                            (string-length "end of file while reading symbol"))))
           (format (current-error-port) (G_ "~aUnterminated extended symbol. Did you mean to use \"# {\" instead of \"#{\"?~%")
                   location))
         (apply throw args)))
    (('syntax-error proc message properties form . rest)
     (let ((loc (or (source-properties->location properties)
                    loc (location file 1 0))))
       (report-error loc (G_ "~a~%  ~y~%") message form)))
    (('unbound-variable _ ...)
     (print-diagnostic-prefix (G_ "error: ") #:colors %error-color)
     (report-unbound-variable-error args #:frame frame))
    ((key args ...)
     (let ((loc (or loc (location file 1 0))))
       (match args
         (((? gwl-error? c) . rest)
          (cond
           ((gwl-package-error? c)
            (report-error loc (G_ "Could not find package `~a' with the current Guix~%")
                          (gwl-package-error-package-spec c)))
           ((gwl-type-error? c)
            (let* ((actual-value (gwl-type-error-actual-value c))
                   (actual-type (class-name (class-of actual-value))))
              (match (gwl-type-error-expected-type c)
                ((and (one . more) types)
                 (report-error loc (G_ "type error: expected one of ~{`~a'~^ ~}, but got `~a' in `~a'~%")
                               types actual-type actual-value))
                (one
                 (report-error loc (G_ "type error: expected `~a' got `~a' in `~a'~%")
                               one actual-type actual-value)))))
           ((message-condition? c)
            (report-error loc (condition-message c)))
           ((formatted-message? c)
            (apply report-error loc
                   (formatted-message-string c)
                   (formatted-message-arguments c))
            (when (fix-hint? c)
              (display-hint (condition-fix-hint c))))))
         (((? symbol? proc) (? string? message) (args ...) . rest)
          (print-diagnostic-prefix (G_ "error: ") #:colors %error-color)
          (display-error frame (current-error-port) proc message
                         args rest))
         (_
          ;; Some exceptions like 'git-error' do not follow Guile's convention
          ;; above and need to be printed with 'print-exception'.
          (print-diagnostic-prefix (G_ "error: ") #:colors %error-color)
          (print-exception (current-error-port) frame key args)))))))

(define (last-frame-with-source stack file)
  "Walk stack upwards and return the last frame that has source location
information in FILE, or #f if it could not be found."
  (define (frame-with-source frame)
    ;; Walk from FRAME upwards until source location information is found.
    (let loop ((frame    frame)
               (previous frame))
      (if (not frame)
          previous
          (if (and (frame-source frame)
                   ;; Restrict to FILE.
                   (string=? file
                             (basename
                              (location-file
                               (source-properties->location (frame-source frame)))))
                   ;; On Guile 3, the latest frame with source may be that of
                   ;; 'raise-exception' in boot-9.scm.  Skip it.
                   (not (eq? 'raise-exception (frame-procedure-name frame))))
              frame
              (loop (frame-previous frame) frame)))))

  (and stack
       (let* ((depth (stack-length stack))
              (last  (and (> depth 0) (stack-ref stack 0))))
         (frame-with-source (if (> depth 1)
                                (stack-ref stack 1) ;skip the 'throw' frame
                                last)))))
