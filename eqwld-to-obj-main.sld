
;; pfs l \* oggok.s3d | while read I; do ../build/bin/pfs e oggok.s3d "$I"; mv "$I" oggok/; done


;; -- use ImageMagicK to convert
;; for file in *.bmp
;; do
;;     convert "$file" "$(basename "$file" .bmp).png"
;; done


;; ~/src/eqwld-obj/eqwld-to-obj-gauche.scm --placeables . oggok_obj.wld 2> err

(define-library (eqwld-to-obj-main)
  (export main-program)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (scheme process-context)
          (srfi 60)
          (srfi 69)
          (snow assert)
          (snow filesys)
          (foldling command-line)
          (seth cout)
          (seth math-3d)
          (seth model-3d)
          (seth obj-model)
          (seth scad-model)
          (seth port-extras)
          (seth ieee-754)
          )
  (begin

    (define (read-byte wld-data i)
      (bytevector-u8-ref wld-data i))

    (define (read-signed-byte wld-data i)
      ;; 2's complement
      (let ((v (read-byte wld-data i)))
        (if (> (bitwise-and v #x80) 0)
            (- (+ (bitwise-xor v #xff) 1))
            v)))

    (define (read-2bytes wld-data i)
      (+ (arithmetic-shift (bytevector-u8-ref wld-data (+ i 1)) 8)
         (bytevector-u8-ref wld-data i)))

    (define (read-signed-word wld-data i)
      ;; 2's complement
      (let ((v (read-2bytes wld-data i)))
        (if (> (bitwise-and v #x8000) 0)
            (- (+ (bitwise-xor v #xffff) 1))
            v)))

    (define (read-dword wld-data i)
      (+ (arithmetic-shift (bytevector-u8-ref wld-data (+ i 3)) 24)
         (arithmetic-shift (bytevector-u8-ref wld-data (+ i 2)) 16)
         (arithmetic-shift (bytevector-u8-ref wld-data (+ i 1)) 8)
         (bytevector-u8-ref wld-data i)))

    (define (read-signed-dword wld-data i)
      ;; 2's complement
      (let ((v (read-dword wld-data i)))
        (if (> (bitwise-and v #x80000000) 0)
            (- (+ (bitwise-xor v #xffffffff) 1))
            v)))

    (define (read-float-dword wld-data i)
      (let ((v (read-dword wld-data i)))
        (ieee-754->number v)))



    (define (read-magic wld-data i)
      ;; Magic : DWORD
      ;; This always contains 0x54503D02. It identifies the file as a .WLD file.
      (= (read-dword wld-data i) #x54503D02))

    (define (read-version wld-data i)
      ;; Version : DWORD
      ;; For old-format .WLD files, this always contains 0x00015500. For new-format .WLD files, this always contains 0x1000C800.
      (read-dword wld-data i))

    (define (read-fragment-count wld-data i)
      ;; FragmentCount : DWORD
      ;; Contains the number of fragments in the .WLD file, minus 1 (that is, the highest fragment index, starting at 0).
      (read-dword wld-data i))

    (define (read-header-3 wld-data i)
      ;; Header3 : DWORD
      ;; Should contain the number of 0x22 BSP Region fragments in the file.
      (read-dword wld-data i))

    (define (read-header-4 wld-data i)
      ;; Header4 : DWORD
      ;; Unknown purpose. Should contain 0x000680D4.
      (read-dword wld-data i))


    (define (read-string-hash-size wld-data i)
      ;; Contains the size of the string hash in bytes.
      (read-dword wld-data i))

    (define (read-header-6 wld-data i)
      ;; Unknown purpose. This is a guess (that seems to work): it should contain the number of fragments in the file,
      ;; minus the number of 0x03 fragments, minus 6.
      (read-dword wld-data i))


    (define string-xor-values (vector #x95 #x3a #xc5 #x2a #x95 #x7a #x95 #x6a))

    (define (read-string-from-hash wld-data string-start-index hash-start-index string-hash)
      (let loop ((result '())
                 (j string-start-index))
        (let* ((xor-value (vector-ref string-xor-values (modulo (- j hash-start-index) 8)))
               (encoded-value (bytevector-u8-ref wld-data j))
               (decoded-value (bitwise-xor encoded-value xor-value)))
          (cond ((= decoded-value 0)
                 ;; done with string
                 (let ((key (- (- hash-start-index j) 1)))
                   (hash-table-set! string-hash key (list->string (reverse result))))
                 (+ j 1))
                (else
                 (loop (cons (integer->char decoded-value) result)
                       (+ j 1)))))))

    (define (read-string-hash wld-data i string-hash-size)
      (let ((string-hash (make-hash-table)))
        (let loop ((j i))
          (cond ((>= j (+ i string-hash-size))
                 ;; done with all strings in string-hash
                 string-hash)
                (else
                 (loop (read-string-from-hash wld-data j i string-hash)))))))


    (define-record-type <fragment-3>
      (make-fragment-3 filenames)
      fragment-3?
      (filenames fragment-3-filenames fragment-3-set-filenames!) ;; list of filenames
      )

    (define (read-fragment-3-texture-filenames wld-data i frag-index old name frags)
      (let* ((step-i-16 (lambda () (let ((orig-i i)) (set! i (+ i 2)) orig-i)))
             (read-2bytes (lambda () (read-2bytes wld-data (step-i-16))))
             (step-i-32 (lambda () (let ((orig-i i)) (set! i (+ i 4)) orig-i)))
             (read-dword (lambda () (read-dword wld-data (step-i-32))))
             ;;
             (filename-count-raw (read-dword))
             (filename-count (if (= filename-count-raw 0) 1 filename-count-raw))
             (filename-hash (make-hash-table)))

        (let loop ((j 0))
          (cond ((= j filename-count) #t)
                (else
                 (let* ((name-length (read-2bytes)))
                   (set! i (read-string-from-hash wld-data i i filename-hash))
                   (loop (+ j 1))))))

        (cerr "fragment 3, frag-index = " frag-index ", size = " filename-count
              ", filenames = " (hash-table-values filename-hash) "\n")
        (vector-set! frags frag-index (make-fragment-3 (hash-table-values filename-hash)))
        #t))


    (define-record-type <fragment-4>
      (make-fragment-4 flags params1 params2 references)
      fragment-4?
      (flags fragment-4-flags fragment-4-set-flags!)
      (params1 fragment-4-params1 fragment-4-set-params1!)
      (params2 fragment-4-params2 fragment-4-set-params2!)
      (references fragment-4-references fragment-4-set-references!)) ;; a vector of fragment-3 references

    (define (read-fragment-4-texture wld-data i frag-index old name frags)
      (let* ((step-i-32 (lambda () (let ((orig-i i)) (set! i (+ i 4)) orig-i)))
             (read-dword (lambda () (read-dword wld-data (step-i-32))))
             ;;
             (flags (read-dword))
             (size (read-dword))
             (params1 (if (> (bitwise-and flags #x04) 0) (read-dword) #f))
             (params2 (if (> (bitwise-and flags #x08) 0) (read-dword) #f))
             )
        (let loop ((j 0)
                   (refs '()))
          (cond ((= j size)
                 (cerr "fragment 4, frag-index = " frag-index ", flags = " flags ", refs = " refs "\n")
                 (vector-set! frags frag-index (make-fragment-4 flags params1 params2 (list->vector (reverse refs)))))
                (else
                 (loop (+ j 1)
                       (cons (- (read-dword) 1) refs)))))))


    (define-record-type <fragment-5>
      (make-fragment-5 reference flags)
      fragment-5?
      (reference fragment-5-reference fragment-5-set-reference!) ;; reference to a fragment-4
      (flags fragment-5-flags fragment-5-set-flags!))

    (define (read-fragment-5-texture wld-data i frag-index old name frags)
      (let* ((step-i-32 (lambda () (let ((orig-i i)) (set! i (+ i 4)) orig-i)))
             (read-dword (lambda () (read-dword wld-data (step-i-32))))
             ;;
             (reference (- (read-dword) 1))
             (flags (read-dword))
             ;; ... etc
             )
        (cerr "fragment 5, frag-index = " frag-index ", flags = " flags "\n")
        (vector-set! frags frag-index (make-fragment-5 reference flags))
        #t))


    (define-record-type <fragment-14>
      (make-fragment-14 flags magic-word refs)
      fragment-14?
      (flags fragment-14-flags fragment-14-set-flags!)
      (magic-word fragment-14-magic-word fragment-14-set-magic-word!)
      (refs fragment-14-refs fragment-14-set-refs!))

    (define (read-fragment-14-object-location wld-data i frag-index old name frags string-hash)
      (let* ((step-i-32 (lambda () (let ((orig-i i)) (set! i (+ i 4)) orig-i)))
             (read-dword (lambda () (read-dword wld-data (step-i-32))))
             (read-signed-dword (lambda () (read-signed-dword wld-data (step-i-32))))
             (read-float-dword (lambda () (read-float-dword wld-data (step-i-32))))
             ;;
             (flags (read-dword)) ;; flag
             (fragment1 (read-signed-dword)) ;; ref
             (magic-word (hash-table-ref/default string-hash fragment1 #f))
             (size1 (read-dword)) ;; entries
             (size2 (read-dword)) ;; entries2
             (fragment2 (read-dword)) ;; ref2
             (params1 (if (> (bitwise-and flags #x01) 0) (read-dword) #f))
             (params2 (if (> (bitwise-and flags #x02) 0) (read-dword) #f))
             (entries1 '())
             (entries2 '())
             )

        (let loop ((j 0))
          (cond ((= j size1) #t)
                (else
                 (let ((sz (read-dword)))
                   (do ((k 0 (+ k 1)))
                       ((= k sz) #t)
                     (let* ((a (read-dword))
                            (b (read-dword)))
                       (set! entries1 (cons (list a b) entries1))))
                   (loop (+ j 1))))))

        (let loop ((j 0))
          (cond ((= j size2) #t)
                (else
                 (let* ((ref (read-dword)))
                   (set! entries2 (cons (- ref 1) entries2))
                   (loop (+ j 1))))))

        (let ((size3 (read-dword))
              (obj-frag (vector-ref frags (car entries2))))
          (cerr "fragment 14, frag-index:" frag-index
                " flags:" flags
                " fragment1:" fragment1
                " magic-word: " magic-word
                ;; " size1: " size1
                ;; " size2: " size2
                ;; " fragment2:" fragment2
                ;; " params1:" params1
                ;; " params2:" params2
                " entries1:" entries1
                " entries2:" entries2
                " obj-frag: " obj-frag
                " size3:" size3
                "\n")

          (vector-set! frags frag-index (make-fragment-14 flags magic-word entries2)))))


    (define (read-fragment-15-object-location wld-data i frag-index old name frags)
      (let* ((step-i-32 (lambda () (let ((orig-i i)) (set! i (+ i 4)) orig-i)))
             (read-dword (lambda () (read-dword wld-data (step-i-32))))
             (read-float-dword (lambda () (read-float-dword wld-data (step-i-32))))
             ;;
             (flags (read-dword))
             (fragment1 (read-dword))
             (x (read-float-dword))
             (y (read-float-dword))
             (z (read-float-dword))
             (rotate-z (read-float-dword))
             (rotate-y (read-float-dword))
             (rotate-x (read-float-dword))
             (params1-0 (read-float-dword))
             (params1-1 (read-float-dword))
             (params1-2 (read-float-dword))
             (scale-y (read-float-dword))
             (scale-x (read-float-dword))
             (fragment2 (read-dword))
             (params2 (read-dword))
             )
        (cerr "fragment 15, frag-index = " frag-index "\n")
        (cerr "  flags = " flags "\n")
        (cerr "  fragment1 = " fragment1 "\n")
        (cerr "  x = " x "\n")
        (cerr "  y = " y "\n")
        (cerr "  z = " z "\n")

        (cerr "  rotate-z = " rotate-z "\n")
        (cerr "  rotate-y = " rotate-y "\n")
        (cerr "  rotate-x = " rotate-x "\n")
        (cerr "  params1-0 = " params1-0 "\n")
        (cerr "  params1-1 = " params1-1 "\n")
        (cerr "  params1-2 = " params1-2 "\n")
        (cerr "  scale-y = " scale-y "\n")
        (cerr "  scale-x = " scale-x "\n")
        (cerr "  fragment2 = " fragment2 "\n")
        (cerr "  params2 = " params2 "\n")
        ))


    (define-record-type <fragment-2d>
      (make-fragment-2d reference flags)
      fragment-2d?
      (reference fragment-2d-reference fragment-2d-set-reference!) ;; reference to a fragment-5 or a fragment-36
      (flags fragment-2d-flags fragment-2d-set-flags!))

    (define (read-fragment-2d-mesh-ref wld-data i frag-index old name frags)
      (let* ((step-i-32 (lambda () (let ((orig-i i)) (set! i (+ i 4)) orig-i)))
             (read-dword (lambda () (read-dword wld-data (step-i-32))))
             ;;
             (reference (- (read-dword) 1))
             (flags (read-dword))
             ;; ... etc
             )
        (cerr "fragment 2d, frag-index = " frag-index ", ref = " reference ", flags = " flags "\n")
        (vector-set! frags frag-index (make-fragment-2d reference flags))
        #t))


    (define-record-type <fragment-30>
      (make-fragment-30 reference flags)
      fragment-30?
      (reference fragment-30-reference fragment-30-set-reference!) ;; reference to a fragment-5
      (flags fragment-30-flags fragment-30-set-flags!))

    (define (read-fragment-30-texture wld-data i frag-index old name frags)
      (let* ((step-i-32 (lambda () (let ((orig-i i)) (set! i (+ i 4)) orig-i)))
             (read-dword (lambda () (read-dword wld-data (step-i-32))))
             (read-float-dword (lambda () (read-float-dword wld-data (step-i-32))))
             ;;
             (flags (read-dword))
             (params1 (read-dword))
             (params2 (read-dword))
             (params3-0 (read-float-dword))
             (params3-1 (read-float-dword))
             (pair-field (if (= flags 0) (begin (read-dword) (read-dword)) #f))
             (reference (- (read-dword) 1))
             )
        (cerr "fragment 30, frag-index = " frag-index ", flags = " flags ", ref = " reference "\n")
        (vector-set! frags frag-index (make-fragment-30 reference flags))
        #t))


    (define-record-type <fragment-31>
      (make-fragment-31 flags textures)
      fragment-31?
      (flags fragment-31-flags fragment-31-set-flags!)
      (textures fragment-31-textures fragment-31-set-textures!)) ;; a vector of fragment-30 textures

    (define (read-fragment-31-textures wld-data i frag-index old name frags)
      (let* ((step-i-32 (lambda () (let ((orig-i i)) (set! i (+ i 4)) orig-i)))
             (read-dword (lambda () (read-dword wld-data (step-i-32))))
             ;;
             (flags (read-dword))
             (size (read-dword)))
        (let loop ((j 0)
                   (refs '()))
          (cond ((= j size)
                 (cerr "fragment 31, frag-index = " frag-index ", flags = " flags ", refs = " refs "\n")
                 (vector-set! frags frag-index (make-fragment-31 flags (list->vector (reverse refs)))))
                (else
                 (loop (+ j 1)
                       (cons (- (read-dword) 1) refs)))))))


    (define-record-type <fragment-36>
      (make-fragment-36 name texture-frag-indices vertices texture-coords vertex-colors
                        tex-coords normals mat-face-counts material-names faces-corners)
      fragment-36?
      (name fragment-36-name fragment-36-set-name!)
      (texture-frag-indices fragment-36-texture-frag-indices fragment-36-set-texture-frag-indices!)
      (vertices fragment-36-vertices fragment-36-set-vertices!)
      (texture-coords fragment-36-texture-coords fragment-36-set-texture-coords!)
      (vertex-colors fragment-36-vertex-colors fragment-36-set-vertex-colors!)
      (tex-coords fragment-36-tex-coords fragment-36-set-tex-coords!)
      (normals fragment-36-normals fragment-36-set-normals!)
      (mat-face-counts fragment-36-mat-face-counts fragment-36-set-mat-face-counts!)
      (material-names fragment-36-material-names fragment-36-set-material-names!)
      (faces-corners fragment-36-faces-corners fragment-36-set-faces-corners!))

    (define (read-fragment-36-mesh-plain wld-data i frag-index old name frags output-type)
      (let* ((recip-127 (/ 1.0 127.0))
             (recip-255 (/ 1.0 256.0))
             ;;
             (step-i-8 (lambda () (let ((orig-i i)) (set! i (+ i 1)) orig-i)))
             (step-i-16 (lambda () (let ((orig-i i)) (set! i (+ i 2)) orig-i)))
             (step-i-32 (lambda () (let ((orig-i i)) (set! i (+ i 4)) orig-i)))
             (read-byte (lambda () (read-byte wld-data (step-i-8))))
             (read-signed-byte (lambda () (read-signed-byte wld-data (step-i-8))))
             (read-2bytes (lambda () (read-2bytes wld-data (step-i-16))))
             (read-signed-word (lambda () (read-signed-word wld-data (step-i-16))))
             (read-dword (lambda () (read-dword wld-data (step-i-32))))
             (read-signed-dword (lambda () (read-signed-dword wld-data (step-i-32))))
             (read-float-dword (lambda () (read-float-dword wld-data (step-i-32))))
             ;;
             (flags (read-dword))
             (fragment1 (- (read-dword) 1)) ;; zero based indexing
             (fragment2 (read-dword))
             (fragment3 (read-dword))
             (fragment4 (read-dword))
             (center-x (read-float-dword))
             (center-y (read-float-dword))
             (center-z (read-float-dword))
             (params-2-0 (read-dword))
             (params-2-1 (read-dword))
             (params-2-2 (read-dword))
             (max-dist (read-float-dword))
             (min-x (read-float-dword))
             (min-y (read-float-dword))
             (min-z (read-float-dword))
             (max-x (read-float-dword))
             (max-y (read-float-dword))
             (max-z (read-float-dword))
             (vertex-count (read-2bytes))
             (tex-coords-count (read-2bytes))
             (normals-count (read-2bytes))
             (color-count (read-2bytes))
             (polygons-count (read-2bytes))
             (vertex-piece-count (read-2bytes))
             (polygon-tex-count (read-2bytes))
             (vertex-tex-count (read-2bytes))
             (size-9 (read-2bytes))
             (raw-scale (read-2bytes))
             (scale (/ 1.0 (inexact (arithmetic-shift 1 raw-scale))))
             ;;
             (texture-frag-indices (fragment-31-textures (vector-ref frags fragment1)))
             (vertices #f)
             (texture-coords #f)
             (vertex-colors #f)
             (tex-coords #f)
             (normals #f)
             (mat-face-counts #f)
             (material-names #f)
             (frag-faces-corners #f)
             )
        (cerr "---\n")
        (cerr "frag-index = " frag-index "\n")
        (cerr "flags = " (number->string flags 16) "\n")
        (cerr "fragment1 = " fragment1 "\n")
        (cerr "center = (" center-x " " center-y " " center-z ")\n")
        (cerr "max-dist = " max-dist "\n")
        (cerr "min = (" min-x " " min-y " " min-z ")\n")
        (cerr "max = (" max-x " " max-y " " max-z ")\n")
        (cerr "vertex-count = " vertex-count "\n")
        (cerr "tex-coords-count = " tex-coords-count "\n")
        (cerr "normals-count = " normals-count "\n")
        (cerr "color-count = " color-count "\n")
        (cerr "polygons-count = " polygons-count "\n")
        (cerr "vertex-piece-count = " vertex-piece-count "\n")
        (cerr "polygon-tex-count = " polygon-tex-count "\n")
        (cerr "vertex-tex-count = " vertex-tex-count "\n")
        (cerr "size-9 = " size-9 "\n")
        (cerr "raw-scale = " raw-scale "\n")
        (cerr "scale = " scale "\n")
        (cerr "texture-frag-indices = " texture-frag-indices "\n")

        ;; vertices
        (let loop ((j 0)
                   (rev-indices '()))
          (cond ((= j vertex-count)
                 (set! vertices (reverse rev-indices)))
                (else
                 (let* ((x-raw (read-signed-word))
                        (y-raw (read-signed-word))
                        (z-raw (read-signed-word))
                        (x (+ center-x (* x-raw scale)))
                        (y (+ center-y (* y-raw scale)))
                        (z (+ center-z (* z-raw scale)))
                        (v (list->vector (map value->pretty-string (list x y z)))))
                   (loop (+ j 1) (cons v rev-indices))))))


        ;; vertex texture coords
        (let loop ((j 0)
                   (rev-tex-coords '()))
          (cond ((= j tex-coords-count)
                 (set! tex-coords (reverse rev-tex-coords)))
                (else
                 (let* ((u-raw (if old (* (read-signed-word) recip-255) (read-signed-dword)))
                        (v-raw (if old (* (read-signed-word) recip-255)  (read-signed-dword)))
                        (tv (list->vector (map value->pretty-string (list u-raw (- 1.0 v-raw))))))
                   (loop (+ j 1) (cons tv rev-tex-coords))))))

        ;; normals
        (let loop ((j 0)
                   (rev-normals '()))
          (cond ((= j normals-count)
                 (set! normals (reverse rev-normals)))
                (else
                 (let* ((x-raw (read-signed-byte))
                        (y-raw (read-signed-byte))
                        (z-raw (read-signed-byte))
                        (x (* x-raw recip-127))
                        (y (* y-raw recip-127))
                        (z (* z-raw recip-127))
                        (v (list->vector (map value->pretty-string (list x y z)))))
                   (loop (+ j 1) (cons v rev-normals))))))

        ;; vertex colors
        ;; -- currently unused
        (if (> color-count 0)
            (let loop ((j 0)
                       (vertex-color-lst '()))
              (cond ((= j color-count)
                     (set! vertex-colors (list->vector (reverse vertex-color-lst))))
                    (else
                     (let* ((color-raw (read-dword))
                            (red (arithmetic-shift (bitwise-and color-raw #xff000000) -24))
                            (green (arithmetic-shift (bitwise-and color-raw #x00ff0000) -16))
                            (blue (arithmetic-shift (bitwise-and color-raw #x0000ff00) -8))
                            (alpha (bitwise-and color-raw #x000000ff)))
                       (loop (+ j 1)
                             (cons (vector (/ (inexact red) 255.0)
                                           (/ (inexact green) 255.0)
                                           (/ (inexact blue) 255.0)
                                           (/ (inexact alpha) 255.0))
                                   vertex-color-lst)))))))

        ;; read in face corners, add them to model later
        (let loop ((j 0)
                   (rev-new-faces-corners '()))
          (cond ((= j polygons-count)
                 (set! frag-faces-corners (reverse rev-new-faces-corners)))
                (else
                   (let* ((face-flags (read-2bytes))
                          (v0 (read-2bytes))
                          (v1 (read-2bytes))
                          (v2 (read-2bytes))
                          ;; same number of normals as vertices, so this works:
                          (face-corner-0 (make-face-corner v0 v0 v0))
                          (face-corner-1 (make-face-corner v1 v1 v1))
                          (face-corner-2 (make-face-corner v2 v2 v2))
                          (face-corners (list face-corner-2 face-corner-1 face-corner-0))
                          (is-phantom (> (bitwise-and face-flags #x10) 0))
                          )
                     (if (or (and (eq? output-type 'phantom) is-phantom)
                             (and (eq? output-type 'solid) (not is-phantom))
                             (eq? output-type 'all))
                         (loop (+ j 1) (cons face-corners rev-new-faces-corners))
                         (loop (+ j 1) (cons '() rev-new-faces-corners)))))))


        ;; jump over VertexPiece entries
        (set! i (+ i (* vertex-piece-count 4)))

        ;; PolygonTex entries
        (let loop ((j 0)
                   (rev-mat-face-count '())
                   (rev-material-names '()))
          (cond ((= j polygon-tex-count)
                 (set! mat-face-counts (reverse rev-mat-face-count))
                 (set! material-names (reverse rev-material-names)))
                (else
                 (let* ((mat-face-count (read-2bytes))
                        (texture-index (read-2bytes))
                        (texture-30-index (vector-ref texture-frag-indices texture-index)) ;; a fragment-30
                        (texture-30 (vector-ref frags texture-30-index))
                        (texture-5-index (fragment-30-reference texture-30))
                        (texture-5 (vector-ref frags texture-5-index))
                        (texture-4-index (fragment-5-reference texture-5))
                        (texture-4 (vector-ref frags texture-4-index))
                        (texture-3-index (vector-ref (fragment-4-references texture-4) 0))
                        (texture-3 (vector-ref frags texture-3-index))
                        (texture-filename (car (fragment-3-filenames texture-3)))
                        (material-name (substring (string-downcase texture-filename) 0 (- (string-length texture-filename) 4))))
                   (cerr "material -- count: " mat-face-count ", name: " material-name "\n")
                   (loop (+ j 1)
                         (cons mat-face-count rev-mat-face-count)
                         (cons material-name rev-material-names))))))

        ;; VertexTex entries
        (let loop ((j 0))
          (cond ((= j vertex-tex-count)
                 #t)
                (else
                 (read-2bytes)
                 (read-2bytes)
                 (loop (+ j 1)))))

        (let loop ((j 0))
          (cond ((= j size-9) #t)
                (else
                 (let* ((vertex-index-1 (read-2bytes))
                        (vertex-index-2 (read-2bytes))
                        (data9-param1 (read-2bytes))
                        (data9-type (read-2bytes)))
                   (cerr "data9: " (list vertex-index-1 vertex-index-2 data9-param1 data9-type) "\n")
                   (loop (+ j 1))))))

        (vector-set! frags frag-index (make-fragment-36 name texture-frag-indices vertices texture-coords vertex-colors
                                                        tex-coords normals mat-face-counts material-names frag-faces-corners))
        #t))


    (define (read-fragment wld-data i frag-index old string-hash frags output-type)
      (let* ((size (read-dword wld-data i))
             (id (read-dword wld-data (+ i 4)))
             (name-key (read-signed-dword wld-data (+ i 8)))
             (name (hash-table-ref/default string-hash name-key "")))

        (cerr "FRAGMENT: size = " size
              ", index = " frag-index
              ", id = " (number->string id 16) ", name = " name-key " = " name "\n")
        ;; (cerr "FRAGMENT: id = " (number->string id 16) "\n")

        (cond
         ((= id #x03)
          (read-fragment-3-texture-filenames wld-data (+ i 12) frag-index old name frags))
         ((= id #x04)
          (read-fragment-4-texture wld-data (+ i 12) frag-index old name frags))
         ((= id #x05)
          (read-fragment-5-texture wld-data (+ i 12) frag-index old name frags))
         ((= id #x14)
          (read-fragment-14-object-location wld-data (+ i 12) frag-index old name frags string-hash))
         ((= id #x15)
          (read-fragment-15-object-location wld-data (+ i 12) frag-index old name frags))
         ((= id #x2d)
          (read-fragment-2d-mesh-ref wld-data (+ i 12) frag-index old name frags))
         ((= id #x30)
          (read-fragment-30-texture wld-data (+ i 12) frag-index old name frags))
         ((= id #x31)
          (read-fragment-31-textures wld-data (+ i 12) frag-index old name frags))
         ((= id #x36)
          (read-fragment-36-mesh-plain wld-data (+ i 12) frag-index old name frags output-type))
         )

        (+ (+ i size) 8)))


    (define (add-fragment-36-to-model frag model)
      (let* ((name (fragment-36-name frag))
             (mesh (make-mesh name '()))
             (vertex-index-start (coordinates-length (model-vertices model)))
             (texture-index-start (coordinates-length (model-texture-coordinates model)))
             (normal-index-start (coordinates-length (model-normals model)))
             (texture-frag-indices (fragment-36-texture-frag-indices frag))
             (vertices (fragment-36-vertices frag))
             (texture-coords (fragment-36-texture-coords frag))
             (vertex-colors (fragment-36-vertex-colors frag))
             (tex-coords (fragment-36-tex-coords frag))
             (normals (fragment-36-normals frag))
             (mat-face-counts (fragment-36-mat-face-counts frag))
             (material-names (fragment-36-material-names frag))
             (frag-faces-corners (fragment-36-faces-corners frag)))

        (for-each
         (lambda (v)
           (model-append-vertex! model (make-vertex v (vector 'unset 'unset 'unset))))
         vertices)

        (for-each
         (lambda (n)
           (model-append-normal! model n))
         normals)

        (for-each
         (lambda (tv)
           (model-append-texture-coordinate! model tv))
         tex-coords)

        (for-each
         (lambda (material-name mat-face-count)
           (let ((material (model-get-material-by-name model material-name)))
             (do ((k 0 (+ k 1)))
                 ((= k mat-face-count) #t)
               ;; (face-set-material! (car frag-faces-corners) material)
               (let ((face-corners (car frag-faces-corners)))
                 (if (not (null? face-corners))
                     (mesh-prepend-face! model mesh face-corners material
                                         vertex-index-start texture-index-start normal-index-start)))
               (set! frag-faces-corners (cdr frag-faces-corners)))))
         material-names mat-face-counts)

        (model-prepend-mesh! model mesh)))


    (define (rotate-model model)
      (let ((rotated-coords (make-coordinates))
            (rot-matrix (matrix-rotation-x (- pi/2))))
        (for-each
         (lambda (vertex)
           (let ((rotated-vertex (vertex-transform rot-matrix vertex)))
             (coordinates-append! rotated-coords rotated-vertex)))
         (vector->list (coordinates-as-vector (model-vertices model))))
        (model-set-vertices! model rotated-coords)))


    (define (main-program)
      (define (usage why)
        (cerr why "\n")
        (cerr "eqwld-to-obj [arguments] lines-input-file points-input-file\n")
        (cerr "    --obj                      output an obj file\n")
        (cerr "    --scad                     output an openscad file\n")
        (cerr "    --phantom                  output parts the character can pass though\n")
        (cerr "    --solid                    output parts the character can collide with\n")
        (cerr "    --placeables <directory>   extract placables from wld file into <directory>\n")
        (exit 1))

      (let* ((args (parse-command-line `((--obj)
                                         (--scad)
                                         (--phantom)
                                         (--solid)
                                         (--placeables output-directory)
                                         (-?) (-h))))
             (output-obj #f)
             (output-scad #f)
             (output-type #f)
             (placeables-output-directory #f)
             (extra-arguments '()))
        (for-each
         (lambda (arg)
           (case (car arg)
             ((-? -h) (usage ""))
             ((--obj)
              (if (or output-obj output-scad)
                  (usage "give only one of: --obj --pnm --scad --texture --caves"))
              (set! output-obj #t))
             ((--scad)
              (if (or output-obj output-scad)
                  (usage "give only one of: --obj --pnm --scad --texture --caves"))
              (set! output-scad #t))
             ((--phantom)
              (if output-type
                  (usage "give --phantom or --solid only once"))
              (set! output-type 'phantom))
             ((--solid)
              (if output-type
                  (usage "give --phantom or --solid only once"))
              (set! output-type 'solid))
             ((--placeables)
              (if placeables-output-directory
                  (usage "give --placeables only once"))
              (set! placeables-output-directory (cadr arg)))
             ((--)
              (set! extra-arguments (cdr arg)))))
         args)

        (if (not (= (length extra-arguments) 1))
            (usage "give wld filename as an argument"))

        (if (not output-type)
            (set! output-type 'all))

        (let* ((wld-filename (car extra-arguments))
               (input-handle (open-input-file wld-filename))
               (wld-data (read-all-u8 input-handle))
               (i 0)
               (step-i (lambda ()
                         (let ((orig-i i))
                           (set! i (+ i 4))
                           orig-i))))
          (cerr "input filename is " wld-filename "\n")

          (cond ((not (read-magic wld-data i))
                 (cerr "bad magic\n")
                 (cerr (number->string (bytevector-u8-ref wld-data (+ i 3)) 16) "\n")
                 (cerr (number->string (bytevector-u8-ref wld-data (+ i 2)) 16) "\n")
                 (cerr (number->string (bytevector-u8-ref wld-data (+ i 1)) 16) "\n")
                 (cerr (number->string (bytevector-u8-ref wld-data (+ i 0)) 16) "\n")
                 (snow-assert #f)))
          (step-i)

          (let* ((version (number->string (read-version wld-data (step-i)) 16))
                 (old #t) ;; XXX based on version
                 (fragment-count (read-fragment-count wld-data (step-i)))
                 (header-3 (read-header-3 wld-data (step-i)))
                 (header-4 (number->string (read-header-4 wld-data (step-i)) 16))
                 (string-hash-size (read-string-hash-size wld-data (step-i)))
                 (header-6 (read-header-6 wld-data (step-i)))
                 (string-hash (read-string-hash wld-data i string-hash-size))
                 (frags (make-vector fragment-count #f))
                 )

            (set! i (+ i string-hash-size))
            (cerr "version = " version "\n")
            (cerr "fragment count = " fragment-count "\n")
            (cerr "header-3 = " header-3 "\n")
            (cerr "header-4 = " header-4 "\n")
            (cerr "string-hash-size = " string-hash-size "\n")
            (cerr "header-6 = " header-6 "\n")

            (let loop ((frag-index 0))
              (cond ((= frag-index fragment-count) #t)
                    (else
                     (set! i (read-fragment wld-data i frag-index old string-hash frags output-type))
                     (loop (+ frag-index 1)))))

            (cond
             (placeables-output-directory
              ;; extrace placeable objects
              (cerr "output placeables to " placeables-output-directory "\n")
              (if (not (snow-file-directory? placeables-output-directory))
                  (snow-create-directory placeables-output-directory))
              ;; 14 --> 2d --> 36
              (do ((frag-index 0 (+ frag-index 1)))
                  ((= frag-index fragment-count) #t)
                (let ((frag (vector-ref frags frag-index)))
                  (cond ((fragment-14? frag)
                         (for-each
                          (lambda (frag-2d-ref)
                            (let* ((frag-2d (vector-ref frags frag-2d-ref))
                                   (frag-36 (vector-ref frags (fragment-2d-reference frag-2d))))
                              (cond ((fragment-36? frag-36)
                                     (let* ((model (make-empty-model))
                                            (output-filename (string-append
                                                              placeables-output-directory "/"
                                                              (fragment-36-name frag-36)
                                                              "-"
                                                              (number->string frag-index)
                                                              ".obj"))
                                            (output-handle (open-output-file output-filename)))
                                       (add-material-library model
                                                             (string-append
                                                              (substring wld-filename 0 (- (string-length wld-filename) 4))
                                                              ".mtl"))
                                       (add-fragment-36-to-model frag-36 model)
                                       (scale-model model 0.2) ;; try to adjust the scale so 1.0 = about a meter
                                       (rotate-model model) ;; make y be up
                                       (write-obj-model model output-handle)
                                       (close-output-port output-handle))))))
                          (fragment-14-refs frag)))))))
             (else
              ;; extract a zone geometry
              (let ((model (make-empty-model)))
                (add-material-library model
                                      (string-append
                                       (substring wld-filename 0 (- (string-length wld-filename) 4))
                                       ".mtl"))
                (do ((frag-index 0 (+ frag-index 1)))
                    ((= frag-index fragment-count) #t)
                  (let ((frag (vector-ref frags frag-index)))
                    (cond ((fragment-36? frag)
                           (add-fragment-36-to-model frag model)))))

                (scale-model model 0.2) ;; try to adjust the scale so 1.0 = about a meter
                (rotate-model model) ;; make y be up
                (write-obj-model model (current-output-port)))))

            ))))
    ))
