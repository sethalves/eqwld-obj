
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; raster utilities -- 2d vector.
;; this makes no assumptions about what's stored inside the raster.
;;

(define-library (seth raster)
  (export raster-new
          raster-copy
          raster-set-pixel!
          raster-set-pixel-if-in-bounds!
          raster-get-pixel
          raster-width
          raster-height
          raster-max
          raster-min
          raster-spiral-search
          )
  (import (scheme base))
  (begin

    (define (raster-new width height . default-pixel)
      (let ((pixel (if (null? default-pixel)
                       (vector 0 0 0 255)
                       (car default-pixel))))
        (let ((rows (make-vector height)))
          (let row-loop ((y 0))
            (cond ((< y height)
                   (vector-set! rows y (make-vector width pixel))
                   (row-loop (+ y 1)))
                  (else rows))))))


    (define (raster-copy img)
      (let* ((height (vector-length img))
             (rows (make-vector height)))
        (let row-loop ((y 0))
          (cond ((< y height)
                 (vector-set! rows y (vector-copy (vector-ref img y)))
                 (row-loop (+ y 1)))
                (else rows)))))


    (define (raster-set-pixel! img x y color)
      (let ((row (vector-ref img y)))
        (vector-set! row x color)
        #t))


    (define (raster-set-pixel-if-in-bounds! img x y color)
      (cond ((< x 0) #f)
            ((< y 0) #f)
            ((>= x (raster-width img)) #f)
            ((>= y (raster-height img)) #f)
            (else (raster-set-pixel! img x y color))))


    (define (raster-get-pixel img x y)
      (let ((row (vector-ref img y)))
        (vector-ref row x)))


    (define (raster-width img)
      (cond ((= (vector-length img) 0) 0)
            (else (vector-length (vector-ref img 0)))))

    (define (raster-height img)
      (vector-length img))


    (define (raster-max img . greater-than-oa)
      ;; find the largest value in img, according to the two-argument
      ;; test-function in (car greater-than-oa).  if greater-than-oa isn't
      ;; provided, use >
      (let ((maximum (raster-get-pixel img 0 0))
            (gt (if (null? greater-than-oa) > (car greater-than-oa)))
            (width (raster-width img))
            (height (raster-height img)))
        (do ((y 0 (+ y 1)))
            ((= y height) maximum)
          (do ((x 0 (+ x 1)))
              ((= x width) maximum)
            (let ((v (raster-get-pixel img x y)))
              (if (gt v maximum)
                  (set! maximum v)
                  #t ;; must have an else
                  ))))))


    (define (raster-min img . less-than-oa)
      ;; find the smallest value in img, according to the two-argument
      ;; test-function in (car less-than-oa).  if less-than-oa isn't
      ;; provided, use <
      (let ((lt (if (null? less-than-oa) < (car less-than-oa))))
        (raster-max img lt)))


    (define (raster-spiral-search pos
                           search-low search-high
                           point-tester)
      ;; This is for doing a 2D search for something near the start point.
      ;; x and y are the start point.
      ;; low and high are vectors that define the rectangular bounds of the search.
      ;; point-tester is called for each pixel.  if should return #f if we
      ;; haven't found what we're looking for, and #t if we have.
      ;;
      ;; 1 up, 1 left, 2 down, 2 right, 3 up, 3 left, 4 down, 4 right, 5 up,
      ;; 5 left, etc
      (let* ((vector2-x (lambda (v) (vector-ref v 0)))
             (vector2-y (lambda (v) (vector-ref v 1)))
             (move-in-dir
              (lambda (pos dir)
                (cond ((eq? dir 'up)
                       (vector (vector2-x pos) (+ (vector2-y pos) 1)))
                      ((eq? dir 'left)
                       (vector (- (vector2-x pos) 1) (vector2-y pos)))
                      ((eq? dir 'down)
                       (vector (vector2-x pos) (- (vector2-y pos) 1)))
                      ((eq? dir 'right)
                       (vector (+ (vector2-x pos) 1) (vector2-y pos)))
                      (else
                       (vector (vector2-x pos) (+ (vector2-y pos) 1))))))
             (call-point-tester
              (lambda (pos)
                (cond ((< (vector2-x pos) (vector2-x search-low)) #f)
                      ((>= (vector2-x pos) (vector2-x search-high)) #f)
                      ((< (vector2-y pos) (vector2-y search-low)) #f)
                      ((>= (vector2-y pos) (vector2-y search-high)) #f)
                      (else (point-tester pos)))))
             ;; think harder about the value of max-n, which is used to
             ;; terminate the search in failure.
             (max-n (floor
                     (* (+ (max (- (vector2-x pos) (vector2-x search-low))
                                (- (vector2-y pos) (vector2-y search-low))
                                (- (vector2-x search-high) (vector2-x pos))
                                (- (vector2-y search-high) (vector2-y pos)))
                           1)
                        2))))
        (let nloop ((n 1)
                    (pos pos))
          (let cloop ((c0 n)
                      (c1 n)
                      (pos pos))
            (cond ((> c0 0)
                   (if (not (call-point-tester pos))
                       (cloop (- c0 1) c1
                              (move-in-dir pos (if (odd? n) 'up 'down)))
                       pos))

                  ((> c1 0)
                   (if (not (call-point-tester pos))
                       (cloop 0 (- c1 1)
                              (move-in-dir pos (if (odd? n) 'left 'right)))
                       pos))
                  ((>= n max-n) #f)
                  (else
                   (nloop (+ n 1) pos)))))))
    ))
