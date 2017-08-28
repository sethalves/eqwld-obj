;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command line argument handling for R7RS Scheme.
;;;
;;; See README, command-line.sld, and getopt-long.sld for more
;;; information.
;;;
;;; This software is written by Evan Hanson <evhan@foldling.org> and
;;; placed in the Public Domain. All warranties are disclaimed.
;;;

(define getopt-long
  (let ((string-index
         (lambda (s c)
           (let ((len (string-length s)))
             (let lp ((i 0))
               (cond ((= i len) #f)
                     ((char=? (string-ref s i) c) i)
                     (else (lp (+ i 1)))))))))
    (lambda (arg grammar)
      (cond ((<= (string-length arg) 3) #f)
            ((not (string=? "--" (string-copy arg 0 2))) #f)
            ((string-index arg #\=) =>
             (lambda (i)
               (cond ((assq (string->symbol (string-copy arg 0 i)) grammar) =>
                      (lambda (spec)
                        (if (null? (cdr spec))
                            (error "Unexpected command line option value" arg)
                            (lambda (args process)
                              (process
                               spec
                               (cons (string-copy arg 0 i)
                                     (cons (string-copy arg (+ i 1))
                                           (cdr args))))))))
                     (else #f))))
            (else #f)))))
