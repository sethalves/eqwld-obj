;;
;; http://netpbm.sourceforge.net/doc/ppm.html
;;
;; (list 0 0 0) is a black pixel
;;

(define-library (seth pbm)
  (export image->ppm
          image->pam
          ppm->image
          ppm-file->image)
  (import (scheme base)
          (scheme read)
          (scheme r5rs)
          (srfi 60)
          (seth cout)
          (seth raster)
          (seth image)
          )
  (begin

    (define (image->ppm img out-port)
      ;; write out a ppm file.
      ;; the image is encoded in a vector of vectors of lists.
      ;; each row is a vector containing red/green/blue lists.
      (define (toint x)
        (if (inexact? x) (exact (round x)) x))
      (let ((width (raster-width img))
            (height (raster-height img))
            (mcv (toint (image-maximum-color-value img rgba))))
        (cout "P6\n" out-port)
        (cout width " " height "\n" out-port)
        (cout mcv "\n" out-port)
        (do ((y (- height 1) (- y 1))) ;; make image axis be lower left
            ((= y -1) #t)
          (do ((x 0 (+ x 1)))
              ((= x width) #t)
            ;; XXX two bytes/channel if mcv is > 255
            ;; XXX need to run pnmgamma on what this makes.
            (let ((pixel (raster-get-pixel img x y)))
              (write-u8 (toint (pixel-red pixel)) out-port)
              (write-u8 (toint (pixel-green pixel)) out-port)
              (write-u8 (toint (pixel-blue pixel)) out-port))))))


    (define (image->pam img out-port)
      ;; write out a netpbm pam file.
      (define (toint x) (if (inexact? x) (exact (round x)) x))
      (let ((width (raster-width img))
            (height (raster-height img))
            (mcv (if (< (image-maximum-color-value img rgba) 256) 255 #f))) ;; XXX
        (cout "P7\n"
              "WIDTH " width "\n"
              "HEIGHT " height "\n"
              "DEPTH 4\n"
              "MAXVAL " mcv "\n"
              "TUPLTYPE RGB_ALPHA\n"
              "ENDHDR\n" out-port)
        (do ((y (- height 1) (- y 1))) ;; make image axis be lower left
            ((= y -1) #t)
          (do ((x 0 (+ x 1)))
              ((= x width) #t)
            ;; XXX two bytes/channel if mcv is > 255
            (let ((pixel (raster-get-pixel img x y)))
              (cout (integer->char (toint (pixel-red pixel)))
                    (integer->char (toint (pixel-green pixel)))
                    (integer->char (toint (pixel-blue pixel)))
                    (integer->char (toint (pixel-alpha pixel))) out-port))))))



    (define (c-read p)
      ;; read a value, ignore # lines
      (let ((c (peek-char p)))
        (cond ((eof-object? c) (read-char p))
              ((char-whitespace? c) (read-char p) (c-read p))
              ((eqv? c #\#) (begin (read-line p) (c-read p)))
              (else (read p)))))


    (define (ppm-P2->image in-port)
      (let* ((width (c-read in-port))
             (height (c-read in-port))
             (max-color-value (c-read in-port))
             (ws (read-char in-port)))
        (cond ((or (eof-object? width)
                   (eof-object? height)
                   (eof-object? max-color-value)
                   (eof-object? ws)
                   (not (number? width))
                   (not (number? height))
                   (not (number? max-color-value))
                   (not (char-whitespace? ws)))
               (cout "ppm: bad header.\n" (current-error-port))
               #f)
              (else
               (let ((img (raster-new width height (vector 0 0 0 255))))
                 (do ((y (- height 1) (- y 1)))
                     ((= y -1) #t)
                   (do ((x 0 (+ x 1)))
                       ((= x width) #t)
                     (let* ((grey (read in-port)))
                       (cond
                        ((eof-object? grey)
                         (cout "reading ppm: eof too soon 1.\n"
                               (current-error-port))
                         #f)
                        (else
                         (raster-set-pixel! img x y (vector grey grey grey 255)))))))
                 img)))))


    (define (ppm-P6->image in-port)
      ;; let* to keep the order correct
      (let* ((width (c-read in-port))
             (height (c-read in-port))
             (max-color-value (c-read in-port))
             (ws (read-char in-port)))
        (cond ((or (eof-object? width)
                   (eof-object? height)
                   (eof-object? max-color-value)
                   (eof-object? ws)
                   (not (number? width))
                   (not (number? height))
                   (not (number? max-color-value))
                   (not (char-whitespace? ws)))
               (cout "ppm: bad header.\n" (current-error-port))
               #f)
              (else
               (let ((img (raster-new width height (vector 0 0 0 255))))
                 ;; XXX two bytes/channel if P6 and max-color-value > 255
                 (do ((y (- height 1) (- y 1)))
                     ((= y -1) #t)
                   (do ((x 0 (+ x 1)))
                       ((= x width) #t)
                     ;; (cout "x=" x " y=" y " pos=" (ftell in-port))
                     ;; let* to keep the order correct
                     (let* ((r (read-u8 in-port))
                            (g (read-u8 in-port))
                            (b (read-u8 in-port)))
                       ;; (cout ", r=" r " g=" g " b=" b "\n")
                       (cond
                        ((or (eof-object? r) (eof-object? g) (eof-object? b))
                         (cout "reading ppm: eof too soon 1.\n"
                               (current-error-port))
                         #f)
                        (else
                         (raster-set-pixel! img x y (vector r g b 255)))))))
                 img)))))



    (define (ppm-P4->image in-port)
      ;; XXX -- this comes out inverted.  FIX ME
      (let ((width (c-read in-port))
            (height (c-read in-port))
            (ws (read-char in-port)))
        (cond ((or (eof-object? width)
                   (eof-object? height)
                   (not (number? width))
                   (not (number? height))
                   (not (char-whitespace? ws)))
               (cout "ppm: bad header.\n" (current-error-port))
               #f)
              (else
               (let ((img (raster-new width height (vector 0 0 0 255)))
                     (on (vector 255 255 255 255)))
                 (do ((y (- height 1) (- y 1)))
                     ((= y -1) img)
                   (do ((x 0 (+ x 8)))
                       ((>= x width) img)
                     (let* ((bits-chr (read-char in-port))
                            (bits (if (char? bits-chr) (char->integer bits-chr) 0)))
                       (cond
                        ((eof-object? bits)
                         (cout "reading ppm: eof too soon 0." (current-error-port))
                         #f)
                        (else
                         (if (> (bitwise-and bits 128) 0)
                             (raster-set-pixel-if-in-bounds! img (+ x 0) y on) #t)
                         (if (> (bitwise-and bits 64) 0)
                             (raster-set-pixel-if-in-bounds! img (+ x 1) y on) #t)
                         (if (> (bitwise-and bits 32) 0)
                             (raster-set-pixel-if-in-bounds! img (+ x 2) y on) #t)
                         (if (> (bitwise-and bits 16) 0)
                             (raster-set-pixel-if-in-bounds! img (+ x 3) y on) #t)
                         (if (> (bitwise-and bits 8) 0)
                             (raster-set-pixel-if-in-bounds! img (+ x 4) y on) #t)
                         (if (> (bitwise-and bits 4) 0)
                             (raster-set-pixel-if-in-bounds! img (+ x 5) y on) #t)
                         (if (> (bitwise-and bits 2) 0)
                             (raster-set-pixel-if-in-bounds! img (+ x 6) y on) #t)
                         (if (> (bitwise-and bits 1) 0)
                             (raster-set-pixel-if-in-bounds! img (+ x 7) y on) #t)
                         ))))))))))


    (define (ppm->image in-port)
      ;; read a ppm file from in-port, return an image
      (let ((tag (c-read in-port)))
        (cond ((equal? tag 'P6) (ppm-P6->image in-port))
              ((equal? tag 'P4) (ppm-P4->image in-port))
              ((equal? tag 'P2) (ppm-P2->image in-port))
              (else
               (cout "ppm: bad header.\n" (current-error-port))
               #f))))


    (define (ppm-file->image in-filename)
      (let* ((p (open-input-file in-filename))
             (img (ppm->image p)))
        (close-input-port p)
        img))
    ))
