;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command line argument handling for R7RS Scheme.
;;;
;;; See README and command-line.scm for more information.
;;;
;;; This software is written by Evan Hanson <evhan@foldling.org> and
;;; placed in the Public Domain. All warranties are disclaimed.
;;;

;;
;; This Scheme library provides a way to collect command line arguments
;; into an association list according to a simple S-expressive options
;; grammar. It's meant to be easy to use and pure-R7RS.
;;
;; One procedure is provided, `parse-command-line`. Refer to the README
;; and command-line.scm for API details.
;;
(define-library (foldling command-line)
  (import (scheme base)
          (scheme case-lambda)
          (scheme process-context))
  (export make-command-line-parser parse-command-line)
  (include "command-line.scm"))
