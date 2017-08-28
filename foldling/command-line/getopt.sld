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
;; This Scheme library provides a `getopt(3)`-style option handler for
;; the `(foldling command-line)` library.
;;
;; It provides one function, `getopt`, that can be used as a matcher for
;; that library's `parse-command-line` procedure.
;;
;; Refer to the README for more details.
;;
(define-library (foldling command-line getopt)
  (import (scheme base)
          (only (foldling command-line) make-command-line-parser))
  (export getopt parse-command-line)
  (include "getopt.scm")
  (begin
    (define parse-command-line
      (make-command-line-parser getopt))))
