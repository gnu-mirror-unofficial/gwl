;;; Copyright © 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gwl cache)
  #:use-module (gwl ui)
  #:use-module ((gwl processes)
                #:select (process-inputs process->script-arguments))
  #:use-module ((gwl workflows)
                #:select (workflow-restrictions))
  #:use-module ((gwl workflows utils)
                #:select (mkdir-p))
  #:use-module ((guix base32)
                #:select (bytevector->base32-string))
  #:use-module ((rnrs bytevectors)
                #:select (string->utf8
                          bytevector->u8-list
                          u8-list->bytevector))
  #:use-module ((rnrs io ports)
                #:select (get-bytevector-all))
  #:use-module ((gcrypt hash)
                #:select (sha256))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%cache-root
            %cache-delay

            process->hash
            process->hash-string

            make-process->cache-prefix
            cache!
            restore!))


;;; Data cache

(define %cache-root (make-parameter "/tmp/gwl"))
(define %cache-delay (make-parameter 0))

(define (process->hash process scripts-table)
  "Return a hash of the process.  The name of the script file already
contains the hash of all its inputs, so we use that instead of the
contents of the script, which we don't want to compute right now.
Since the same scripts may be called with different arguments, we
*also* need to include the process parameters in this computation.

SCRIPTS-TABLE is a hash table of script file names for any given
process."
  (let ((script+arguments
         (format #false "~a ~a"
                 (hash-ref scripts-table process)
                 (process->script-arguments process))))
    (bytevector->u8-list
     (sha256 (string->utf8 script+arguments)))))

(define process->hash-string
  (compose bytevector->base32-string
           u8-list->bytevector
           process->hash))

(define (hash-input-file file-name)
  "Return a hash representing the input file FILE-NAME.  We don't
actually hash all the file, because that might be very expensive
--- especially when done *every* time a workflow is supposed to be
run.  We just hash the file name, the mtime and the file size and hope
that's enough.

We should probably have a cache for file hashes, so that hashing even
large files can be acceptable as it only has to be done once."
  (let ((st (stat file-name)))
    (bytevector->u8-list
     (sha256 (string->utf8 (format #f "~a~a~a"
                                   file-name
                                   (stat:mtime st)
                                   (stat:size st)))))))

(define (workflow->data-hashes workflow processes free-inputs-map scripts-table)
  "Return an alist associating each of the WORKFLOW's ordered list of
PROCESSES with the hash of all the process scripts used to generate
their outputs.  FREE-INPUTS-MAP is an alist of input names to file
names that must be considered when computing the hash."
  (define graph (workflow-restrictions workflow))
  (define (process-free-inputs process)
    (filter-map (lambda (input)
                  (and=> (assoc-ref free-inputs-map input) first))
                (process-inputs process)))
  (define input-hashes
    (map (match-lambda
           ((name file-name)
            (cons file-name (hash-input-file file-name))))
         free-inputs-map))
  ;; Compute hashes for chains of scripts.
  (define (kons process acc)
    (cons (cons process
                (append (process->hash process scripts-table)
                        ;; Hash of mapped free inputs.
                        (append-map (cut assoc-ref acc <>)
                                    (process-free-inputs process))
                        ;; All outputs of processes this one depends on.
                        (append-map (cut assoc-ref acc <>)
                                    (or (assoc-ref graph process) '()))))
          acc))
  (map (match-lambda
         ((process . hashes)
          (cons process
                (bytevector->base32-string
                 (sha256
                  (u8-list->bytevector hashes))))))
       (fold kons input-hashes processes)))

(define (make-process->cache-prefix workflow free-inputs-map ordered-processes
                                    scripts-table)
  "Return a procedure that takes a process and returns the cache
prefix for its outputs."
  (let ((hashes (workflow->data-hashes workflow
                                       ;; Ensure flat list of processes
                                       (append-map (match-lambda
                                                     ((? list? l) l)
                                                     (l (list l)))
                                                   ordered-processes)
                                       free-inputs-map
                                       scripts-table)))
    (lambda (process)
      (and=> (assoc-ref hashes process)
             (cut string-append (%cache-root) "/" <> "/")))))

(define (directory? file)
  (and=> (stat file #f)
         (lambda (st) (eq? 'directory (stat:type st)))))

(define (link-or-symlink source target)
  ;; Cross-device links don't work, of course, so we try symlinking
  ;; on error.
  (catch 'system-error
    (lambda ()
      (link source target))
    (lambda (key . args)
      (if (string=? "link" (car args))
          (symlink source target)
          (throw key args)))))

(define (cache! file cache-prefix)
  "Cache FILE by linking it to the directory CACHE-PREFIX."
  (unless (directory? file)
    (mkdir-p (string-append cache-prefix (dirname file)))
    ;; TODO: cross-device links don't work, of course.  Copy?  Symlink?
    ;; Make this configurable?
    (let ((cached-file (string-append cache-prefix file)))
      (when (file-exists? cached-file)
        (delete-file cached-file))
      (log-event 'cache (G_ "Caching `~a' as `~a'~%")
                 file cached-file)
      (link-or-symlink file cached-file))))

(define (restore! file cache-prefix)
  "Restore FILE from the cache at CACHE-PREFIX."
  (unless (directory? file)
    (mkdir-p (dirname file))
    (when (file-exists? file)
      (delete-file file))
    (log-event 'cache (G_ "Restoring `~a' to `~a'~%")
               (string-append cache-prefix file) file)
    (link-or-symlink (string-append cache-prefix file) file)))
