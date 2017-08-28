
(define-library (seth octree)
  (export make-octree
          octree?
          octree-contents octree-set-contents!
          octree-children octree-set-children!
          octree-bounds octree-set-bounds!
          octree-add-element!
          octree-ray-intersection
          octree-triangle-intersection
          )
  (import (scheme base)
          (snow assert)
          (seth math-3d)
          )
  (begin

    (define-record-type <octree>
      (make-octree~ contents children bounds)
      octree?
      (contents octree-contents octree-set-contents!)
      (children octree-children octree-set-children!)
      (bounds octree-bounds octree-set-bounds!))


    (define (make-octree bounds)
      (snow-assert (aa-box? bounds))
      (make-octree~ '() (vector #f #f #f #f #f #f #f #f) bounds))


    (define (octree-get-child octree nth child-bounds)
      (let* ((children (octree-children octree))
             (child (vector-ref children nth)))
        (if child
            child
            (let ((new-child (make-octree child-bounds)))
              (vector-set! children nth new-child)
              new-child))))


    (define (octree-add-element! octree element element-aa-box)
      (snow-assert (octree? octree))
      (snow-assert (aa-box-contains-aa-box (octree-bounds octree) element-aa-box))

      ;; children are like this:
      ;; - - -  0
      ;; - - +  1
      ;; - + -  2
      ;; - + +  3
      ;; + - -  4
      ;; + - +  5
      ;; + + -  6
      ;; + + +  7

      (let* ((bounds (octree-bounds octree))
             (center (vector3-scale (vector3-sum (aa-box-low-corner bounds)
                                                 (aa-box-high-corner bounds))
                                    0.5))
             (x- (vector3-x (aa-box-low-corner bounds)))
             (y- (vector3-y (aa-box-low-corner bounds)))
             (z- (vector3-z (aa-box-low-corner bounds)))
             (x0 (vector3-x center))
             (y0 (vector3-y center))
             (z0 (vector3-z center))
             (x+ (vector3-x (aa-box-high-corner bounds)))
             (y+ (vector3-y (aa-box-high-corner bounds)))
             (z+ (vector3-z (aa-box-high-corner bounds)))
             (bounds--- (make-aa-box (vector x- y- z-) (vector x0 y0 z0)))
             (bounds--+ (make-aa-box (vector x- y- z0) (vector x0 y0 z+)))
             (bounds-+- (make-aa-box (vector x- y0 z-) (vector x0 y+ z0)))
             (bounds-++ (make-aa-box (vector x- y0 z0) (vector x0 y+ z+)))
             (bounds+-- (make-aa-box (vector x0 y- z-) (vector x+ y0 z0)))
             (bounds+-+ (make-aa-box (vector x0 y- z0) (vector x+ y0 z+)))
             (bounds++- (make-aa-box (vector x0 y0 z-) (vector x+ y+ z0)))
             (bounds+++ (make-aa-box (vector x0 y0 z0) (vector x+ y+ z+))))
        (cond ((aa-box-contains-aa-box bounds--- element-aa-box)
               (octree-add-element! (octree-get-child octree 0 bounds---)
                                    element element-aa-box))
              ((aa-box-contains-aa-box bounds--+ element-aa-box)
               (octree-add-element! (octree-get-child octree 1 bounds--+)
                                    element element-aa-box))
              ((aa-box-contains-aa-box bounds-+- element-aa-box)
               (octree-add-element! (octree-get-child octree 2 bounds-+-)
                                    element element-aa-box))
              ((aa-box-contains-aa-box bounds-++ element-aa-box)
               (octree-add-element! (octree-get-child octree 3 bounds-++)
                                    element element-aa-box))
              ((aa-box-contains-aa-box bounds+-- element-aa-box)
               (octree-add-element! (octree-get-child octree 4 bounds+--)
                                    element element-aa-box))
              ((aa-box-contains-aa-box bounds+-+ element-aa-box)
               (octree-add-element! (octree-get-child octree 5 bounds+-+)
                                    element element-aa-box))
              ((aa-box-contains-aa-box bounds++- element-aa-box)
               (octree-add-element! (octree-get-child octree 6 bounds++-)
                                    element element-aa-box))
              ((aa-box-contains-aa-box bounds+++ element-aa-box)
               (octree-add-element! (octree-get-child octree 7 bounds+++)
                                    element element-aa-box))
              (else
               ;; doesn't fit into any children, leave the element in this node
               (octree-set-contents! octree (cons element (octree-contents octree))))))

      )


    (define (octree-ray-intersection octree segment)
      (snow-assert (octree? octree))
      (snow-assert (vector? segment))
      (snow-assert (= (vector-length segment) 2))
      (snow-assert (= (vector-length (vector-ref segment 0)) 3))
      (snow-assert (= (vector-length (vector-ref segment 1)) 3))
      (if (not (segment-aa-box-intersection segment (octree-bounds octree)))
          '()
          (apply append (list octree)
                 (map (lambda (child)
                        (if child
                            (octree-ray-intersection child segment)
                            '()))
                      (vector->list (octree-children octree))))))


    (define (octree-triangle-intersection octree T)
      ;; (aa-box-intersects-aa-box box0 box1)
      (snow-assert (octree? octree))
      (snow-assert (triangle? T))
      (let ((T-aa-box (make-aa-box (triangle-p0 T) (triangle-p0 T))))
        (aa-box-add-point! T-aa-box (triangle-p1 T))
        (aa-box-add-point! T-aa-box (triangle-p2 T))
        (if (not (aa-box-intersects-aa-box T-aa-box (octree-bounds octree)))
            '()
            (apply append (list octree)
                   (map (lambda (child)
                          (if child
                              (octree-triangle-intersection child T)
                              '()))
                        (vector->list (octree-children octree)))))))

    ))
