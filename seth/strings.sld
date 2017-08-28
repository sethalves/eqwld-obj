(define-library (seth strings)
  (export string-starts-with?
          string-ends-with?
          string-split)
  (import (scheme base)
          (srfi 13)
          (snow assert))

  (begin

    (cond-expand
     (chicken
      ;; chicken's srif-13 is missing this
      (define (reverse-list->string char-list)
        (let* ((len (length char-list))
               (s (make-string len)))
          (let loop ((i (- len 1))
                     (char-list char-list))
            (cond ((null? char-list) s)
                  (else
                   (string-set! s i (car char-list))
                   (loop (- i 1)
                         (cdr char-list))))))))
     (else))


    (define (string-starts-with? s starting . tester-oa)
      (let ((tester (if (null? tester-oa) equal? (car tester-oa)))
            (len-s (string-length s))
            (len-starting (string-length starting)))
        (if (> len-starting len-s)
            #f
            (tester (substring s 0 len-starting) starting))))


    (define (string-ends-with? s ending . tester-oa)
      (let ((tester (if (null? tester-oa) equal? (car tester-oa)))
            (len-s (string-length s))
            (len-ending (string-length ending)))
        (if (> len-ending len-s)
            #f
            (equal? (substring s (- len-s len-ending) len-s) ending))))


    (define (string-split-with-tester str tester)
      (snow-assert (string? str))
      (snow-assert (procedure? tester))
      (let loop ((chars (string->list str))
                 (result '())
                 (results '()))
        (cond ((null? chars)
               (reverse (cons (list->string (reverse result)) results)))
              ((tester (car chars))
               (loop (cdr chars)
                     '()
                     (cons (list->string (reverse result)) results)))
              (else
               (loop (cdr chars)
                     (cons (car chars) result)
                     results)))))


    (define (string-split-with-string str delim)
      (snow-assert (string? str))
      (let loop ((ret (list))
                 (this-part '())
                 (str str))
        (cond
         ((not str) #f)
         ((= (string-length str) 0)
          (reverse (cons (reverse-list->string this-part) ret)))
         ((string-starts-with? str delim)
          (loop (cons (reverse-list->string this-part) ret) ""
                (substring str (string-length delim) (string-length str))))
         (else
          (loop ret
                (cons (string-ref str 0) this-part)
                (substring str 1 (string-length str)))))))


    ;; srfi-13's string-tokenize will skip empty tokens.
    (define (string-split str with-what)
      (snow-assert (string? str))
      (cond ((procedure? with-what)
             (string-split-with-tester str with-what))
            ((char? with-what)
             (string-split-with-tester str (lambda (c) (eqv? c #\/))))
            ((string? with-what)
             (string-split-with-string str with-what))
            (else
             (error "string-split: bad with-what argument" with-what))))

    ))
