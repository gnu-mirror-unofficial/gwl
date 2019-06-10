;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl workflows graph)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (gwl processes)
  #:use-module (gwl workflows)
  #:use-module (gwl workflows utils)
  #:use-module ((guix packages) #:select (package-full-name package?))
  #:export (workflow->dot))

;;; ---------------------------------------------------------------------------
;;; GRAPHING FUNCTIONALITY
;;; ---------------------------------------------------------------------------

(define take-color (color-scheme-stepper %modern-color-scheme))

(define (workflow-dot-prettify-node process)
  "Returns a string of prettified node names for a Graphviz graph."
  (let ((pretty-name (string-map (lambda (x)
                                   (if (eq? x #\-) #\  x))
                                 (process-name process))))
    (format #f " ~s [shape=box,style=\"rounded,filled\",\
fontname=\"helvetica\",\
fillcolor=~s,\
label=<<FONT POINT-SIZE=\"14\">~a</FONT><BR/>\
<FONT POINT-SIZE=\"12\">~a<BR/>~a</FONT>>];~%"
            (process-full-name process)
            (take-color)
            (string-upcase pretty-name)
            (process-synopsis process)
            (match (process-packages process)
              (() "")
              (inputs (format #f "<BR/>Uses: ~{~a~^, ~}."
                              (map package-full-name inputs)))))))

(define (workflow-restriction->dot process . restrictions)
  "Write the dependency relationships of a restriction in dot format."
  (format #f "~{~a~}~%"
          (map (lambda (item)
                 (format #f "~s -> ~s~%"
                         (process-full-name item)
                         (process-full-name process)))
               restrictions)))

(define* (workflow->dot workflow #:key (parallel? #t))
  "Returns the workflow's processes formatted in Graphviz's Dot language as a
directed acyclic graph."
  (format #f "digraph G {~%  graph [bgcolor=transparent, fontsize=24];~%~{~a~}~%~{~a~}}"
          (map workflow-dot-prettify-node (workflow-processes workflow))
          (map (lambda (restriction)
                 (apply workflow-restriction->dot restriction))
               (workflow-restrictions workflow))))
