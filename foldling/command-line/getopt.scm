;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command line argument handling for R7RS Scheme.
;;;
;;; See README, command-line.sld, and getopt.sld for more information.
;;;
;;; This software is written by Evan Hanson <evhan@foldling.org> and
;;; placed in the Public Domain. All warranties are disclaimed.
;;;

(define (getopt arg grammar)
  (cond ((<= (string-length arg) 2) #f)
        ((not (char=? #\- (string-ref arg 0))) #f)
        ((assq (string->symbol (string-copy arg 0 2)) grammar) =>
         (lambda (spec)
           (lambda (args process)
             (process
              spec
              (cons (string-copy arg 0 2)
                    (if (null? (cdr spec))
                        (cons (let ((s (string-copy arg 2)))
                                (if (char=? (string-ref s 0) #\-)
                                    s
                                    (string-append "-" s)))
                              (cdr args))
                        (cons (string-copy arg 2)
                              (cdr args))))))))
        (else #f)))
