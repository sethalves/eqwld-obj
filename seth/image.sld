
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; raster image utilities
;;
;;

(define-library (seth image)
  (export pixel-nth-channel
          pixel-red
          pixel-green
          pixel-blue
          pixel-alpha
          pixel-channel
          pixel-set-channel!
          pixel-set-red!
          pixel-set-green!
          pixel-set-blue!
          pixel-set-alpha!
          pixel-list-channels
          pixel-maximum-color-value
          pixel-minimum-color-value
          image-best-color-value
          image-maximum-color-value
          image-minimum-color-value
          pixel-=
          pixel-non-black?
          pixel-/
          pixel-combine
          pixel-+
          pixel--
          image-fast-pixel-+
          pixel-replace-channels
          image-pixel-combine!
          image-pixel-sum!
          image-pixel-diff!
          image-set-pixel!
          image-rectangle-opr!
          image-anti-alias
          image-mask
          image-non-black-pixel?
          image-non-black-neighbors
          image-erode
          image-rectangle-set!
          image-rectangle-sum!
          image-flood-fill!
          image-scale
          pixel-copy
          image-crop
          image-all-of-color-v?
          image-all-of-color-h?
          image-left-margin
          image-right-margin
          image-bottom-margin
          image-top-margin
          image-autocrop
          image-copy
          image-combine!
          image-+!
          image--!
          image-point-is-in-bounds
          ;; text->image
          image-circle!
          image-line!
          rgba)
  (import (scheme base)
          (scheme inexact)
          (seth raster)
          )
  (begin

    (define pi 3.14159265358979323846)
    (define pi*2 (* pi 2.0))
    (define pi/2 (/ pi 2.0))

    (define rgba (vector 'red 'green 'blue 'alpha))

    (define (pixel-nth-channel pxl n) (vector-ref pxl n))
    (define (pixel-red pxl) (pixel-nth-channel pxl 0))
    (define (pixel-green pxl) (pixel-nth-channel pxl 1))
    (define (pixel-blue pxl) (pixel-nth-channel pxl 2))
    (define (pixel-alpha pxl) (pixel-nth-channel pxl 3))
    (define (pixel-channel pxl channel)
      (case channel
        ((red) (vector-ref pxl 0))
        ((green) (vector-ref pxl 1))
        ((blue) (vector-ref pxl 2))
        ((alpha) (vector-ref pxl 3))))


    (define (pixel-set-channel! pxl n val) (vector-set! pxl n val))
    (define (pixel-set-red! pxl val) (pixel-set-channel! pxl 0 val))
    (define (pixel-set-green! pxl val) (pixel-set-channel! pxl 1 val))
    (define (pixel-set-blue! pxl val) (pixel-set-channel! pxl 2 val))
    (define (pixel-set-alpha! pxl val) (pixel-set-channel! pxl 3 val))


    (define (pixel-list-channels pxl channels)
      ;; list the values in the channels of pixel
      (vector-map (lambda (channel) (pixel-channel pxl channel)) channels))


    (define (pixel-maximum-color-value pxl channels)
      ;; channels is a list of symbols to check.  ex: (list 'red 'green).
      ;; at least one channel must be listed
      (apply max (vector->list (pixel-list-channels pxl channels))))


    (define (pixel-minimum-color-value pxl channels)
      ;; channels is a list of symbols to check.  ex: (list 'red 'green).
      ;; at least one channel must be listed
      (apply min (vector->list (pixel-list-channels pxl channels))))


    (define (image-best-color-value img channels tester extractor)
      ;; search image for color value that trumps others according to tester
      ;; extractor gets the values from a pixel.  this is support function
      ;; for image-maximum-color-value and image-minimum-color-value.
      (let row-loop ((y 0) (row-best 0))
        (cond ((< y (vector-length img))
               (let ((row (vector-ref img y)))
                 (let col-loop ((x 0) (col-best row-best))
                   (cond ((< x (vector-length row))
                          (let* ((pixel (vector-ref row x))
                                 (m (extractor pixel channels)))
                            (cond ((tester m col-best) (col-loop (+ x 1) m))
                                  (else (col-loop (+ x 1) col-best)))))
                         (else (row-loop (+ y 1) col-best))))))
              (else row-best))))


    (define (image-maximum-color-value img channels)
      ;; scan a raster and find the largest color number.
      ;; if channels is provided, it should be a list of
      ;; symbols like (list 'red 'green 'blue) indicating
      ;; which channels are of interest.
      (image-best-color-value img channels > pixel-maximum-color-value))


    (define (image-minimum-color-value img channels)
      ;; scan a raster and find the largest color number.
      ;; if channels is provided, it should be a list of
      ;; symbols like (list 'red 'green 'blue) indicating
      ;; which channels are of interest.
      (image-best-color-value img channels < pixel-minimum-color-value))

    (define (almost= a b tolerance)
      (<= (abs (- a b)) tolerance))

    (define (vector-member m v)
      (let loop ((i 0))
        (cond ((>= i (vector-length v)) #f)
              ((equal? (vector-ref v i) m) #t)
              (else (loop (+ i 1))))))

    (define (pixel-= pxl-a pxl-b channels . fudge-factor-oa)
      ;; (equal? (pixel-list-channels pxl-a channels)
      ;;         (pixel-list-channels pxl-b channels)))
      (let ((fudge-factor (if (null? fudge-factor-oa) 0 (car fudge-factor-oa))))
        (and
         (if (vector-member 'red channels)
             (almost= (vector-ref pxl-a 0) (vector-ref pxl-b 0) fudge-factor)
             #t)
         (if (vector-member 'green channels)
             (almost= (vector-ref pxl-a 1) (vector-ref pxl-b 1) fudge-factor)
             #t)
         (if (vector-member 'blue channels)
             (almost= (vector-ref pxl-a 2) (vector-ref pxl-b 2) fudge-factor)
             #t)
         (if (vector-member 'alpha channels)
             (almost= (vector-ref pxl-a 3) (vector-ref pxl-b 3) fudge-factor)
             #t))))


    (define (pixel-non-black? pxl)
      (or (> (pixel-red pxl) 0)
          (> (pixel-green pxl) 0)
          (> (pixel-blue pxl) 0)))


    (define (pixel-/ orig-pxl v channels)
      (vector-map (lambda (value channel)
                    (if (vector-member channel channels) (/ value v) value))
                  orig-pxl rgba))


    (define (pixel-combine orig-pxl inc-pxl min-value max-value channels opr)
      (vector-map (lambda (orig-value inc-value channel)
                    (if (vector-member channel channels)
                        (let ((new-value (opr orig-value inc-value)))
                          (cond ((< new-value min-value) min-value)
                                ((> new-value max-value) max-value)
                                (else new-value)))
                        orig-value))
                  orig-pxl inc-pxl rgba))


    (define (pixel-+ orig-pxl inc-pxl min-value max-value channels)
      (pixel-combine orig-pxl inc-pxl min-value max-value channels +))

    (define (pixel-- orig-pxl inc-pxl min-value max-value channels)
      (pixel-combine orig-pxl inc-pxl min-value max-value channels -))


    (define (image-fast-pixel-+ channels)
      ;; returns a fast adder with no range checking
      (cond ((equal? channels rgba)
             (lambda (pxl-a pxl-b)
               (vector-map (lambda (a b) (+ a b)) pxl-a pxl-b)))
            ((equal? channels (vector 'alpha))
             (lambda (pxl-a pxl-b)
               (vector (vector-ref pxl-a 0)
                       (vector-ref pxl-a 1)
                       (vector-ref pxl-a 2)
                       (+ (vector-ref pxl-a 3) (vector-ref pxl-b 3)))))
            (else pixel-+)))


    (define (pixel-replace-channels orig-pxl new-pxl channels)
      (vector-map (lambda (orig-value new-value channel)
                    (if (vector-member channel channels)
                        new-value
                        orig-value))
                  orig-pxl new-pxl rgba))



    (define (image-pixel-combine! img x y inc-pxl min-value max-value channels opr)
      (let* ((old-pxl (raster-get-pixel img x y))
             (new-pxl (opr old-pxl inc-pxl min-value max-value channels)))
        (raster-set-pixel! img x y new-pxl)))


    (define (image-pixel-sum! img x y inc-pxl min-value max-value channels)
      ;; change the pixel in img at x,y by adding the values in the
      ;; channels of inc-pixel.  clamp the new pixel between min and max-value
      ;; inclusivly.  if inc-pixel is a number, add this number to the
      ;; red, green and blue channels of the pixel in img.
      (image-pixel-combine! img x y inc-pxl min-value max-value channels pixel-+))


    (define (image-pixel-diff! img x y inc-pxl min-value max-value channels)
      (image-pixel-combine! img x y inc-pxl min-value max-value channels pixel--))


    (define (image-set-pixel! img x y pxl channels)
      (let* ((old-pxl (raster-get-pixel img x y)))
        (raster-set-pixel! img x y (pixel-replace-channels old-pxl pxl channels))))


    (define (image-rectangle-opr! img low-x low-y width height opr)
      ;; call opr on all pixels in a rectangular area
      (let ((x-stop (+ low-x width))
            (y-stop (+ low-y height)))
        (do ((x low-x (+ x 1)))
            ((>= x x-stop) img)
          (do ((y low-y (+ y 1)))
              ((>= y y-stop) #t)
            (opr img x y)))))


    (define (image-anti-alias img sample-width channels)
      (let* ((width (raster-width img))
             (height (raster-height img))
             (new-img (raster-new width height))
             (sample-pixels (* (+ (* sample-width 2) 1)
                               (+ (* sample-width 2) 1)))
             (fast-pixel-+ (image-fast-pixel-+ channels)))

        (define (get-pixel x y)
          (let ((xx (cond ((< x 0) 0) ((>= x width) (- width 1)) (else x)))
                (yy (cond ((< y 0) 0) ((>= y width) (- width 1)) (else y))))
            (raster-get-pixel img xx yy)))

        (define (get-aa-value x y)
          (let ((acc (vector 0.0 0.0 0.0 0.0)))
            (do ((xx (- x sample-width) (+ xx 1)))
                ((> xx (+ x sample-width))
                 ;; done...
                 (pixel-replace-channels (raster-get-pixel img x y)
                                         (pixel-/ acc sample-pixels channels)
                                         channels))
              (do ((yy (- y sample-width) (+ yy 1)))
                  ((> yy (+ y sample-width)) #t)
                (set! acc (fast-pixel-+ acc (get-pixel xx yy)))))))

        (image-rectangle-opr!
         new-img 0 0 width height
         (lambda (new-img x y)
           (raster-set-pixel! new-img x y (get-aa-value x y))))))


    (define (image-mask img)
      ;; return white pixels for any non-black pixel
      (let* ((width (raster-width img))
             (height (raster-height img))
             (new-img (raster-new width height)))
        (image-rectangle-opr!
         new-img 0 0 width height
         (lambda (new-img x y)
           (let ((pxl (raster-get-pixel img x y)))
             (if (pixel-non-black? pxl)
                 (raster-set-pixel! new-img x y (vector 255 255 255 255))
                 #t))))))


    (define (image-non-black-pixel? img x y)
      (pixel-non-black? (raster-get-pixel img x y)))


    (define (image-non-black-neighbors img x y)
      ;; how many neighbors of x,y (in 8 directions) aren't black
      (let* ((width (raster-width img))
             (height (raster-height img)))
        (let loop ((dx -1)
                   (dy -1)
                   (total 0))
          (cond ((> dx 1) (loop -1 (+ dy 1) total))
                ((> dy 1) total)
                ((and (= dx 0) (= dy 0)) (loop (+ dx 1) dy total))
                (else
                 (let ((xp (+ x dx))
                       (yp (+ y dy)))
                   (if (and (>= xp 0)
                            (>= yp 0)
                            (< xp width)
                            (< yp height))
                       (loop (+ dx 1) dy
                             (+ total (if (image-non-black-pixel? img xp yp) 1 0)))
                       (loop (+ dx 1) dy total))))))))


    (define (image-erode img)
      ;; return white pixels for any non-black pixel
      (let* ((width (raster-width img))
             (height (raster-height img))
             (new-img (raster-new width height)))
        (image-rectangle-opr!
         new-img 0 0 width height
         (lambda (new-img x y)
           (if (and (image-non-black-pixel? img x y)
                    (> (image-non-black-neighbors img x y) 4))
               (raster-set-pixel! new-img x y (vector 255 255 255 255))
               #t)))))


    (define (image-rectangle-set! img pxl low-x low-y width height channels)
      ;; paint a rectangle pxl
      (image-rectangle-opr! img low-x low-y width height
                            (lambda (img x y)
                              (image-set-pixel! img x y pxl channels))))


    (define (image-rectangle-sum! img inc-pxl low-x low-y width height
                                  min-value max-value channels)
      ;; add inc-color to a rectangular area
      (let ((func (lambda (func-img x y)
                    (image-pixel-sum!
                     func-img x y inc-pxl min-value max-value channels))))
        (image-rectangle-opr! img low-x low-y width height func)))


    (define (image-flood-fill! img target-color end-color start-x start-y
                               sense-channels change-channels)
      ;; flood fill all pixels of color target-color, making them be color
      ;; end-color.  start at point (start-x start-y).  return (list (list
      ;; min-x min-y) (list max-x max-y)) which notes the bounding-box of the
      ;; fill operation.  if no pixels were changed, this will be (list (vector
      ;; #f #f) (vector #f #f)).  sense-channels is used to determine which
      ;; pixels are considered to be like target-color, change-channels
      ;; controls which channels of said pixels are changed.
      (let loop ((stack (list (cons start-x start-y)))
                 (min-x #f) ;; keep track of bounding box of fill
                 (max-x #f)
                 (min-y #f)
                 (max-y #f))
        (if (null? stack) (list (vector min-x min-y) (vector max-x max-y))
            (let* ((next-pos (car stack))
                   (x (car next-pos))
                   (y (cdr next-pos)))
              (cond
               ((< x 0) (loop (cdr stack) min-x max-x min-y max-y))
               ((< y 0) (loop (cdr stack) min-x max-x min-y max-y))
               ((> x (- (raster-width img) 1))
                (loop (cdr stack) min-x max-x min-y max-y))
               ((> y (- (raster-height img) 1))
                (loop (cdr stack) min-x max-x min-y max-y))
               ((pixel-= (raster-get-pixel img x y) target-color sense-channels)
                (image-set-pixel! img x y end-color change-channels)
                (let ((right (cons (+ x 1) y))
                      (up (cons x (+ y 1)))
                      (left (cons (- x 1) y))
                      (down (cons x (- y 1)))
                      (new-min-x (if (or (not min-x) (< x min-x)) x min-x))
                      (new-min-y (if (or (not min-y) (< y min-y)) y min-y))
                      (new-max-x (if (or (not max-x) (> x max-x)) x max-x))
                      (new-max-y (if (or (not max-y) (> y max-y)) y max-y)))
                  (loop (cons down (cons left (cons up (cons right (cdr stack)))))
                        new-min-x new-max-x new-min-y new-max-y)))
               (else
                (loop (cdr stack) min-x max-x min-y max-y)))))))


    (define (image-scale img new-width new-height)
      ;;
      ;;
      ;;
      (let* ((width (raster-width img))
             (height (raster-height img))
             (new-img (raster-new new-width new-height)))

        (define (compute-pixel x-range y-range)
          (define (toint v) (exact (floor v)))
          ;; XXX make this smarter.
          (let ((x-orig (* (/ (+ (car x-range) (cadr x-range)) 2.0) width))
                (y-orig (* (/ (+ (car y-range) (cadr y-range)) 2.0) height)))
            (raster-get-pixel img (toint x-orig) (toint y-orig))))

        (do ((x 0 (+ x 1)))
            ((= x new-width) new-img)
          (do ((y 0 (+ y 1)))
              ((= y new-height) #t)
            (let ((x-range (list (/ x new-width) (/ (+ x 1) new-width)))
                  (y-range (list (/ y new-height) (/ (+ x 1) new-height))))
              (raster-set-pixel! new-img x y (compute-pixel x-range y-range)))))))


    (define (pixel-copy pxl channels)
      (vector-map (lambda (value channel)
                    (if (vector-member channel channels) value 0))
                  pxl rgba))


    (define (image-crop img big-x big-y new-width new-height channels)
      ;; return a rectangular subset of img
      (let ((new-img (raster-new new-width new-height)))
        (do ((x 0 (+ x 1)))
            ((= x new-width) new-img)
          (do ((y 0 (+ y 1)))
              ((= y new-height) #t)
            (let ((pxl (raster-get-pixel img (+ x big-x) (+ y big-y))))
              (image-set-pixel! new-img x y pxl channels))))))


    (define (image-all-of-color-v? img x pxl channels)
      ;; look in column x of img for a pixel unlike pxl
      (let ((height (raster-height img)))
        (let loop ((y 0))
          (if (= y height) #t
              (if (pixel-= (raster-get-pixel img x y) pxl channels)
                  (loop (+ y 1))
                  #f)))))

    (define (image-all-of-color-h? img y pxl channels)
      ;; look in row y of img for a pixel unlike pxl
      (let ((width (raster-width img)))
        (let loop ((x 0))
          (if (= x width) #t
              (if (pixel-= (raster-get-pixel img x y) pxl channels)
                  (loop (+ x 1))
                  #f)))))


    (define (image-left-margin img pxl channels)
      ;; figure out how much of left margin is colored like pxl
      (let ((width (raster-width img)))
        (let loop ((x 0))
          (if (= x width) width
              (if (image-all-of-color-v? img x pxl channels)
                  (loop (+ x 1))
                  x)))))

    (define (image-right-margin img pxl channels)
      ;; figure out how much of right margin is colored like pxl
      (let ((width (raster-width img)))
        (let loop ((x 0))
          (if (= x width) width
              (if (image-all-of-color-v? img (- (- width x) 1) pxl channels)
                  (loop (+ x 1))
                  x)))))

    (define (image-bottom-margin img pxl channels)
      ;; figure out how much of top margin is colored like pxl
      (let ((height (raster-height img)))
        (let loop ((y 0))
          (if (= y height) height
              (if (image-all-of-color-h? img y pxl channels)
                  (loop (+ y 1))
                  y)))))

    (define (image-top-margin img pxl channels)
      ;; figure out how much of right margin is colored like pxl
      (let ((height (raster-height img)))
        (let loop ((y 0))
          (if (= y -1) height
              (if (image-all-of-color-h? img (- (- height y) 1) pxl channels)
                  (loop (+ y 1))
                  y)))))


    (define (image-autocrop img border-size channels)
      ;; look at pixel at (0 0) and remove margins of that color.  only
      ;; channels will be considered when deciding if a pixel can be cropped.
      (let* ((pxl (raster-get-pixel img 0 0))
             (left-margin (- (image-left-margin img pxl channels) border-size))
             (bottom-margin (- (image-bottom-margin img pxl channels) border-size))
             (right-margin (- (image-right-margin img pxl channels) border-size))
             (top-margin (- (image-top-margin img pxl channels) border-size))
             ;;
             (l-margin (if (>= left-margin 0) left-margin 0))
             (b-margin (if (>= bottom-margin 0) bottom-margin 0))
             (r-margin (if (>= right-margin 0) right-margin 0))
             (t-margin (if (>= top-margin 0) top-margin 0))
             ;;
             (width (raster-width img))
             (height (raster-height img)))
        (if (= left-margin width) img
            (image-crop img l-margin b-margin
                        (- (- width l-margin) r-margin)
                        (- (- height b-margin) t-margin) channels))))


    (define (image-copy img channels)
      ;; return a copy of img
      (image-crop img 0 0 (raster-width img) (raster-height img) channels))


    (define (image-combine! big-img small-img big-x big-y
                            min-value max-value operator channels)
      ;; change channels of big-img by applying small-img to them with operator.
      (let ((small-width (raster-width small-img))
            (small-height (raster-height small-img))
            (big-width (raster-width big-img))
            (big-height (raster-height big-img)))
        (do ((x 0 (+ x 1)))
            ((or (>= x small-width) (>= (+ big-x x) big-width)) big-img)
          (do ((y 0 (+ y 1)))
              ((or (>= y small-height) (>= (+ big-y y) big-height)) #t)
            (if (and (>= (+ big-x x) 0) (>= (+ big-y y) 0))
                (operator big-img (+ big-x x) (+ big-y y)
                          (raster-get-pixel small-img x y)
                          min-value max-value channels)
                #t)))))


    (define (image-+! big-img small-img big-x big-y min-value max-value channels)
      (image-combine! big-img small-img big-x big-y
                      min-value max-value image-pixel-sum! channels))


    (define (image--! big-img small-img big-x big-y min-value max-value channels)
      (image-combine! big-img small-img big-x big-y
                      min-value max-value image-pixel-diff! channels))


    (define (image-point-is-in-bounds img x y)
      (and (>= x 0)
           (>= y 0)
           (< x (raster-width img))
           (< y (raster-height img))))


    ;; (define (text->image text)
    ;;   ;; use pbmtext to create letters in an image
    ;;   (let ((tmp-file (string-append "/tmp/text-to-image-"
    ;;                                  (number->string (getpid)))))

    ;;     ;; (system (string-append "echo '" text "' | /usr/bin/pbmtext > " tmp-file))
    ;;     (system (string-append "echo '" text "' | pbmtext > " tmp-file))

    ;;     (let* ((in-port (open-input-file tmp-file))
    ;;            (text-img (ppm->image in-port)))
    ;;       (close-input-port in-port)
    ;;       (delete-file tmp-file)
    ;;       text-img)))


    (define (image-circle! img pxl center-x center-y radius channels)
      ;; radius should be an exact number
      (let ((r2 (* radius radius))
            (y-stop (* (sin (/ pi*2 8)) radius)))
        (do ((y 0 (+ y 1)))
            ((>= y y-stop) img)
          (let ((x (exact (round (sqrt (- r2 (* y y)))))))
            (if (and (< (+ center-x x) (raster-width img))
                     (< (+ center-y y) (raster-height img)))
                (image-set-pixel! img (+ center-x x) (+ center-y y) pxl channels)
                #t)
            (if (and (>= (- center-x x) 0)
                     (< (+ center-y y) (raster-height img)))
                (image-set-pixel! img (- center-x x) (+ center-y y) pxl channels)
                #t)
            (if (and (< (+ center-x x) (raster-width img))
                     (>= (- center-y y) 0))
                (image-set-pixel! img (+ center-x x) (- center-y y) pxl channels)
                #t)
            (if (and (>= (- center-x x) 0)
                     (>= (- center-y y) 0))
                (image-set-pixel! img (- center-x x) (- center-y y) pxl channels)
                #t)

            (if (and (< (+ center-y x) (raster-height img))
                     (< (+ center-x y) (raster-width img)))
                (image-set-pixel! img (+ center-x y) (+ center-y x) pxl channels)
                #t)
            (if (and (>= (- center-y x) 0)
                     (< (+ center-x y) (raster-width img)))
                (image-set-pixel! img (+ center-x y) (- center-y x) pxl channels)
                #t)
            (if (and (< (+ center-y x) (raster-height img))
                     (>= (- center-x y) 0))
                (image-set-pixel! img (- center-x y) (+ center-y x) pxl channels)
                #t)
            (if (and (>= (- center-y x) 0)
                     (>= (- center-x y) 0))
                (image-set-pixel! img (- center-x y) (- center-y x) pxl channels)
                #t)))))


    (define (image-line! img pxl x0 y0 x1 y1 channels . point-drawer-oa)
      ;; http://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
      (let ((dx (abs (- x1 x0)))
            (dy (abs (- y1 y0)))
            (sx (if (< x0 x1) 1 -1))
            (sy (if (< y0 y1) 1 -1))
            (point-drawer (if (null? point-drawer-oa)
                              image-set-pixel!
                              (car point-drawer-oa))))
        (let loop ((x x0)
                   (y y0)
                   (err (- dx dy)))
          (if (and (>= x 0) (< x (raster-width img))
                   (>= y 0) (< y (raster-height img)))
              (point-drawer img x y pxl channels) #f)
          (if (or (not (= x x1)) (not (= y y1)))
              (let ((e2 (* err 2.0)))
                (cond ((and (> e2 (- dy)) (< e2 dx))
                       (loop (+ x sx) (+ y sy) (- (+ err dx) dy)))
                      ((> e2 (- dy))
                       (loop (+ x sx) y (- err dy)))
                      ((< e2 dx)
                       (loop x (+ y sy) (+ err dx)))
                      (else
                       (error "image-line! failed")
                       img)))
              img))))


    ;; anti-aliased lines:
    ;; http://en.wikipedia.org/wiki/Xiaolin_Wu%27s_line_algorithm
    ))
