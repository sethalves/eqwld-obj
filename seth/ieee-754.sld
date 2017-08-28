
(define-library (seth ieee-754)
  (export ieee-754->number
          number->ieee-754)
  (import (scheme base)
          (snow assert)
          (srfi 60))
  (begin

    (define (ieee-754->number bits)
      (cond ;; scheme2js can't parse the +inf.0
       ;; ((= bits #x7f800000) +inf.0)
       ;; ((= bits #xff800000) -inf.0)
       ;; ((= (bitwise-and bits #xff800000) #xff800000) -nan.0)
       ;; ((= (bitwise-and bits #x7f800000) #x7f800000) +nan.0)
       (else
        (let* ((sign-bit (arithmetic-shift (bitwise-and bits #x80000000) -31))
               (exponent (arithmetic-shift (bitwise-and bits #x7f800000) -23))
               (fraction (bitwise-and bits #x007fffff))
               (fraction-as-number
                (let loop ((i #x00400000)
                           (v (/ 1.0 2.0))
                           (result 0))
                  (if (= i 0)
                      (if (= exponent 0) result (+ 1.0 result))
                      (loop (arithmetic-shift i -1)
                            (/ v 2.0)
                            (if (> (bitwise-and fraction i) 0)
                                (+ result v)
                                result))))))
          ;; (cout "---------\n")
          ;; (cout "sign-bit=" sign-bit "\n")
          ;; (cout "exponent=" exponent "\n")
          ;; (cout "fraction=" fraction "\n")
          ;; (cout "fraction-as-number=" fraction-as-number "\n")
          (* (if (= sign-bit 0) 1.0 -1.0)
             (expt 2 (- exponent 127)) fraction-as-number)))))


    (define (number->ieee-754 f32)
      (cond ;; ((eqv? f32 +inf.0) #x7f800000)
       ;; ((eqv? f32 -inf.0) #xff800000)
       ;; ((eqv? f32 +nan.0) #x7f800001)
       ((eqv? f32 0) #x00000000)
       ((eqv? f32 0.0) #x00000000)
       (else
        (let* ((sign-bit (if (< f32 0) 1 0))
               (f32 (if (< f32 0) (- f32) f32))
               )
          (let loop ((f32-shifted f32)
                     (exponent 0))
            (cond ((< f32-shifted 1.0)
                   (loop (* f32-shifted 2.0) (- exponent 1)))
                  ((>= f32-shifted 2.0)
                   (loop (/ f32-shifted 2.0) (+ exponent 1)))
                  (else
                   (let loop ((fraction (- f32-shifted 1.0))
                              (fraction-bits 0)
                              (pow2 #x400000))
                     (if (> pow2 0)
                         (cond ((>= (* fraction 2.0) 1.0)
                                (loop (- (* fraction 2.0) 1.0)
                                      (bitwise-ior pow2 fraction-bits)
                                      (arithmetic-shift pow2 -1)))
                               (else
                                (loop (* fraction 2.0)
                                      fraction-bits
                                      (arithmetic-shift pow2 -1))))
                         ;; done
                         (begin
                           ;; (cout "sign="
                           ;;       (number->string sign-bit 2) "\n")
                           ;; (cout "exponent="
                           ;;       (number->string (+ exponent 127) 2) "\n")
                           ;; (cout "fraction="
                           ;;       (number->string fraction-bits 2) "\n")
                           (bitwise-ior
                            (arithmetic-shift sign-bit 31)
                            (arithmetic-shift (+ exponent 127) 23)
                            fraction-bits))
                         )))))))))

    ))
