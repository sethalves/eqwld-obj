;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command line argument handling for R7RS Scheme.
;;;
;;; See README and command-line.sld for more information.
;;;
;;; This software is written by Evan Hanson <evhan@foldling.org> and
;;; placed in the Public Domain. All warranties are disclaimed.
;;;

;;
;; This Scheme library provides a `getopt_long(3)`-style option handler
;; for the `(foldling command-line)` library.
;;
;; It provides one function, `getopt-long`, that can be used as a
;; matcher for that library's `parse-command-line` procedure.
;;
;; Refer to the README for more details.
;;
(define-library (foldling command-line getopt-long)
  (import (scheme base)
          (only (foldling command-line) make-command-line-parser)
          (only (foldling command-line getopt) getopt))
  (export getopt-long parse-command-line)
  (include "getopt-long.scm")
  (begin

    ;; While getopt-long.scm defines a matcher for double-dashed options
    ;; only, the `getopt-long` procedure exported by this library
    ;; implements the combined behavior of that and the standard
    ;; (single-dashed) `getopt` matcher.
    (define getopt-long
      (let ((getopt-long getopt-long))
        (lambda (arg grammar)
          (or (getopt-long arg grammar)
              (getopt arg grammar)))))

    (define parse-command-line
      (make-command-line-parser getopt-long))))
