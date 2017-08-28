(define-library (seth scad-model)
  (export make-scad-translate
          scad-translate?
          scad-translate-translation
          scad-translate-set-translation!
          scad-translate-children
          scad-translate-set-children!

          make-scad-rotate
          scad-rotate?
          scad-rotate-rotation
          scad-rotate-set-rotation!
          scad-rotate-children
          scad-rotate-set-children!

          make-scad-intersection
          scad-intersection?
          scad-intersection-children
          scad-intersection-set-children!

          make-scad-circle
          scad-circle?
          scad-circle-radius
          scad-circle-set-radius!
          scad-circle-fn
          scad-circle-set-fn!

          make-scad-polygon
          scad-polygon?
          scad-polygon-points
          scad-polygon-set-points!
          scad-polygon-convexity
          scad-polygon-set-convexity!

          make-scad-sphere
          scad-sphere?
          scad-sphere-radius
          scad-sphere-set-radius!
          scad-sphere-fn
          scad-sphere-set-fn!

          make-scad-cube
          scad-cube?
          scad-cube-size
          scad-cube-set-size!
          scad-cube-center
          scad-cube-set-center!

          make-scad-polyhedron
          scad-polyhedron?
          scad-polyhedron-points
          scad-polyhedron-set-points!
          scad-polyhedron-faces
          scad-polyhedron-set-faces!
          scad-polyhedron-convexity
          scad-polyhedron-set-convexity!

          make-scad-rotate-extrude
          scad-rotate-extrude?
          scad-rotate-extrude-angle
          scad-rotate-extrude-set-angle!
          scad-rotate-extrude-convexity
          scad-rotate-extrude-set-convexity!
          scad-rotate-extrude-children
          scad-rotate-extrude-set-children!

          write-scad-file
          model->scad-polyhedron)

  (import (scheme base)
          (scheme file)
          (scheme write)
          (scheme cxr)
          (scheme process-context)
          (srfi 13)
          (srfi 29)
          (srfi 69)
          (snow assert)
          (snow input-parse)
          (seth cout)
          (seth strings)
          (seth math-3d)
          (seth model-3d))
  (cond-expand
   (chicken (import (extras)))
   (else))

  (begin

    (define-record-type <scad-translate>
      (make-scad-translate translation children)
      scad-translate?
      (translation scad-translate-translation scad-translate-set-translation!)
      (children scad-translate-children scad-translate-set-children!))

    (define-record-type <scad-rotate>
      (make-scad-rotate rotation children)
      scad-rotate?
      (rotation scad-rotate-rotation scad-rotate-set-rotation!)
      (children scad-rotate-children scad-rotate-set-children!))

    (define-record-type <scad-intersection>
      (make-scad-intersection children)
      scad-intersection?
      (children scad-intersection-children scad-intersection-set-children!))

    (define-record-type <scad-circle>
      (make-scad-circle radius fn)
      scad-circle?
      (radius scad-circle-radius scad-circle-set-radius!)
      (fn scad-circle-fn scad-circle-set-fn!))

    (define-record-type <scad-polygon>
      (make-scad-polygon points convexity)
      scad-polygon?
      (points scad-polygon-points scad-polygon-set-points!)
      (convexity scad-polygon-convexity scad-polygon-set-convexity!))

    (define-record-type <scad-sphere>
      (make-scad-sphere radius fn)
      scad-sphere?
      (radius scad-sphere-radius scad-sphere-set-radius!)
      (fn scad-sphere-fn scad-sphere-set-fn!)
      ;; (fa scad-sphere-fa scad-sphere-set-fa!)
      ;; (fs scad-sphere-fs scad-sphere-set-fs!)
      )

    (define-record-type <scad-cube>
      (make-scad-cube size center)
      scad-cube?
      (size scad-cube-size scad-cube-set-size!)
      (center scad-cube-center scad-cube-set-center!))


    (define-record-type <scad-polyhedron>
      (make-scad-polyhedron points faces convexity)
      scad-polyhedron?
      (points scad-polyhedron-points scad-polyhedron-set-points!)
      (faces scad-polyhedron-faces scad-polyhedron-set-faces!)
      (convexity scad-polyhedron-convexity scad-polyhedron-set-convexity!))


    (define-record-type <scad-rotate-extrude>
      (make-scad-rotate-extrude angle convexity children)
      scad-rotate-extrude?
      (angle scad-rotate-extrude-angle scad-rotate-extrude-set-angle!)
      (convexity scad-rotate-extrude-convexity scad-rotate-extrude-set-convexity!)
      (children scad-rotate-extrude-children scad-rotate-extrude-set-children!))


    (define (spaces indent)
      (list->string (make-list indent #\space)))


    (define (write-scad-vector v indent port)
      (cout "[" port)
      (write-scad-item (vector-ref v 0) indent port)
      (do ((i 1 (+ i 1)))
          ((= i (vector-length v)) #t)
        (cout ", " port)
        (write-scad-item (vector-ref v i) indent port))
      (cout "]" port))

    (define (write-scad-vertex vertex indent port)
      (let ((v (vertex-position vertex)))
        (cout "[" port)
        (write-scad-item (vector-ref v 0) indent port)
        (do ((i 1 (+ i 1)))
            ((= i (vector-length v)) #t)
          (cout ", " port)
          (write-scad-item (vector-ref v i) indent port))
        (cout "]" port)))

    (define (write-scad-translate trans indent port)
      (cout (spaces indent) "translate(" port)
      (write-scad-item
       (scad-translate-translation trans)
       indent port)
      (cout ") {\n" port)
      (write-scad-items (scad-translate-children trans)
                        (+ indent 4) port)
      (cout (spaces indent) "}\n"))

    (define (write-scad-rotate rotate indent port)
      (snow-assert (scad-rotate? rotate))
      (snow-assert (number? indent))
      (snow-assert (output-port? port))
      (cout (spaces indent) "rotate(" port)
      (let ((rot (scad-rotate-rotation rotate)))
        (write-scad-item
         (if (= (vector-length rot) 4)
             (radians->degrees (quaternion->euler~zyx rot))
             (radians->degrees rot))
         indent port))
      (cout ") {\n" port)
      (write-scad-items (scad-rotate-children rotate)
                        (+ indent 4) port)
      (cout (spaces indent) "}\n"))


    (define (write-scad-intersection intersection indent port)
      (snow-assert (scad-intersection? intersection))
      (snow-assert (number? indent))
      (snow-assert (output-port? port))
      (cout (spaces indent) "intersection() {\n" port)
      (write-scad-items (scad-intersection-children intersection)
                        (+ indent 4) port)
      (cout (spaces indent) "}\n"))


    (define (write-scad-circle circle indent port)
      ;; circle(2, $fn=50);
      (cout (spaces indent) "circle("
            (scad-circle-radius circle) ", $fn="
            (scad-circle-fn circle) ");\n"))


    (define (write-scad-polygon polygon indent port)
      ;; polygon(points = [ [x, y], ... ], paths = [ [p1, p2, p3..], ...], convexity = N);
      (snow-assert (scad-polygon? polygon))
      (snow-assert (output-port? port))
      (cout (spaces indent) "polygon(\n" (spaces indent) "    points = " port)
      (write-scad-item (scad-polygon-points polygon) indent port)
      (cout ",\n" (spaces indent)
            "    convexity = "
            (scad-polygon-convexity polygon) ");\n" port))


    (define (write-scad-sphere sphere indent port)
      ;; sphere($fn = 0, $fa = 12, $fs = 2, r = 1);
      (cout (spaces indent) "sphere("
            (scad-sphere-radius sphere)
            ", $fn=" (scad-sphere-fn sphere)
            ;; ", $fa=" (scad-sphere-fa sphere)
            ;; ", $fs=" (scad-sphere-fs sphere)
            ");\n" port))

    (define (write-scad-cube cube indent port)
      (cout (spaces indent) "cube(" port)
      (write-scad-items (scad-cube-size cube) indent port)
      (cout ", center=" (if (scad-cube-center cube) "true" "false")
            ");\n" port))


    (define (write-scad-polyhedron polyhedron indent port)
      (snow-assert (scad-polyhedron? polyhedron))
      (snow-assert (output-port? port))
      ;; polyhedron( points = [ [X0, Y0, Z0], [X1, Y1, Z1], ... ], faces = [ [P0, P1, P2, P3, ...], ... ], convexity = N);
      (cout (spaces indent) "polyhedron(\n" (spaces indent) "    points = " port)
      (write-scad-item (scad-polyhedron-points polyhedron) indent port)
      (cout ",\n" (spaces indent) "    faces = " port)
      (write-scad-item (scad-polyhedron-faces polyhedron) indent port)
      (cout ",\n" (spaces indent)
            "    convexity = "
            (scad-polyhedron-convexity polyhedron) ");\n" port))


    (define (write-scad-rotate-extrude rotate-extrude indent port)
      ;; rotate_extrude(angle = 360, convexity = 2) { ... }
      (snow-assert (scad-rotate-extrude? rotate-extrude))
      (snow-assert (output-port? port))
      (cout (spaces indent)
            "rotate_extrude(angle="
            (radians->degrees (scad-rotate-extrude-angle rotate-extrude))
            ", convexity=" (scad-rotate-extrude-convexity rotate-extrude)
            ") {\n" port)
      (write-scad-items (scad-rotate-extrude-children rotate-extrude)
                        (+ indent 4) port)
      (cout (spaces indent) "}\n"))


    (define (write-scad-item item indent port)
      (cond ((number? item)
             ;; (cout (format "~a" item) port)
             (cout (number->pretty-string item 6) port)
             )
            ((string? item) (cout item port))
            ((vector? item) (write-scad-vector item indent port))
            ((vertex? item) (write-scad-vertex item indent port))
            ((list? item) (write-scad-item (list->vector item) indent port))
            ((scad-translate? item) (write-scad-translate item indent port))
            ((scad-rotate? item) (write-scad-rotate item indent port))
            ((scad-intersection? item) (write-scad-intersection item indent port))
            ((scad-circle? item) (write-scad-circle item indent port))
            ((scad-polygon? item) (write-scad-polygon item indent port))
            ((scad-sphere? item) (write-scad-sphere item indent port))
            ((scad-cube? item) (write-scad-cube item indent port))
            ((scad-polyhedron? item) (write-scad-polyhedron item indent port))
            ((scad-rotate-extrude? item) (write-scad-rotate-extrude item indent port))
            (else
             (cerr "unknown openscad top-level: " item "\n"))))


    (define (write-scad-items items indent port)
      (if (list? items)
          (for-each
           (lambda (item) (write-scad-item item indent port))
           items)
          (write-scad-item items indent port)))


    (define (write-scad-file top-levels port)
      (snow-assert (list? top-levels))
      (snow-assert (output-port? port))
      (for-each
       (lambda (top-level) (write-scad-items top-level 0 port))
       top-levels))


    (define (model->scad-polyhedron model)
      (snow-assert (model? model))
      (let ((points (vector->list (coordinates-as-vector (model-vertices model))))
            (faces '())
            (convexity 10))
        (operate-on-faces
         model
         (lambda (mesh face)
           (set! faces (cons
                        (vector-map
                         (lambda (face-corner)
                           (snow-assert (face-corner? face-corner))
                           (face-corner-vertex-index face-corner))
                         (list->vector (reverse (vector->list (face-corners face)))))
                        faces))
           face))
        (make-scad-polyhedron points faces convexity)))


    ))
