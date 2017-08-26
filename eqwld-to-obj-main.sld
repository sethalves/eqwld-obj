


;; -- use ImageMagicK to convert
;; for file in *.bmp
;; do
;;     convert "$file" "$(basename "$file" .bmp).png"
;; done


(define-library (eqwld-to-obj-main)
  (export main-program)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (scheme read)
          (scheme write)
          (scheme process-context)
          (scheme cxr)
          (srfi 1)
          (srfi 60)
          (srfi 69)
          (srfi 95)
          (snow assert)
          (foldling command-line)
          (seth cout)
          (seth strings)
          (seth math-3d)
          (seth raster)
          (seth image)
          (seth pbm)
          (seth graph)
          (seth model-3d)
          (seth obj-model)
          (seth scad-model)
          (seth octree)
          (seth port-extras)
          (seth ieee-754)
          (seth model-3d)
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

    (define (read-word wld-data i)
      (+ (arithmetic-shift (bytevector-u8-ref wld-data (+ i 1)) 8)
         (bytevector-u8-ref wld-data i)))

    (define (read-signed-word wld-data i)
      ;; 2's complement
      (let ((v (read-word wld-data i)))
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

    (define (read-fragment-3-texture-filenames wld-data i frag-index old name model frags)
      (let* ((step-i-16 (lambda () (let ((orig-i i)) (set! i (+ i 2)) orig-i)))
             (read-word (lambda () (read-word wld-data (step-i-16))))
             (step-i-32 (lambda () (let ((orig-i i)) (set! i (+ i 4)) orig-i)))
             (read-dword (lambda () (read-dword wld-data (step-i-32))))
             ;;
             (filename-count-raw (read-dword))
             (filename-count (if (= filename-count-raw 0) 1 filename-count-raw))
             (filename-hash (make-hash-table)))

        (let loop ((j 0))
          (cond ((= j filename-count) #t)
                (else
                 (let* ((name-length (read-word)))
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

    (define (read-fragment-4-texture wld-data i frag-index old name model frags)
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

    (define (read-fragment-5-texture wld-data i frag-index old name model frags)
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


    (define-record-type <fragment-30>
      (make-fragment-30 reference flags)
      fragment-30?
      (reference fragment-30-reference fragment-30-set-reference!) ;; reference to a fragment-5
      (flags fragment-30-flags fragment-30-set-flags!))


    (define (read-fragment-30-texture wld-data i frag-index old name model frags)
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

    (define (read-fragment-31-textures wld-data i frag-index old name model frags)
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


    (define (read-fragment-36-mesh-plain wld-data i frag-index old name model frags)
      (let* ((recip-127 (/ 1.0 127.0))
             (recip-255 (/ 1.0 255.0))
             ;;
             (step-i-8 (lambda () (let ((orig-i i)) (set! i (+ i 1)) orig-i)))
             (step-i-16 (lambda () (let ((orig-i i)) (set! i (+ i 2)) orig-i)))
             (step-i-32 (lambda () (let ((orig-i i)) (set! i (+ i 4)) orig-i)))
             (read-byte (lambda () (read-byte wld-data (step-i-8))))
             (read-signed-byte (lambda () (read-signed-byte wld-data (step-i-8))))
             (read-word (lambda () (read-word wld-data (step-i-16))))
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
             (vertex-count (read-word))
             (tex-coords-count (read-word))
             (normals-count (read-word))
             (color-count (read-word))
             (polygons-count (read-word))
             (vertex-piece-count (read-word))
             (polygon-tex-count (read-word))
             (vertex-tex-count (read-word))
             (size-9 (read-word))
             (raw-scale (read-word))
             (scale (/ 1.0 (inexact (arithmetic-shift 1 raw-scale))))
             ;;
             (mesh (make-mesh name '()))
             (vertex-index-start (coordinates-length (model-vertices model)))
             (texture-index-start (coordinates-length (model-texture-coordinates model)))
             (normal-index-start (coordinates-length (model-normals model)))
             (new-vertex-indexes #f)
             (vertex-colors #f)
             (tex-coords #f)
             (textures (fragment-31-textures (vector-ref frags fragment1)))
             (new-faces-corners #f)
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
        (cerr "textures = " textures "\n")

        ;; vertices
        (let loop ((j 0)
                   (rev-new-vertex-indexes '()))
          (cond ((= j vertex-count)
                 (set! new-vertex-indexes (reverse rev-new-vertex-indexes)))
                (else
                 (let* ((x-raw (read-signed-word))
                        (y-raw (read-signed-word))
                        (z-raw (read-signed-word))
                        (x (+ center-x (* x-raw scale)))
                        (y (+ center-y (* y-raw scale)))
                        (z (+ center-z (* z-raw scale)))
                        (v (list->vector (map value->pretty-string (list x y z))))
                        (new-index (model-append-vertex! model (make-vertex v (vector 'unset 'unset 'unset)))))
                   (loop (+ j 1)
                         (cons new-index rev-new-vertex-indexes))))))


        ;; vertex texture coords
        (do ((j 0 (+ j 1)))
            ((= j tex-coords-count) #t)
          (let* ((u-raw (if old (* (read-signed-word) recip-255) (read-signed-dword)))
                 (v-raw (if old (* (read-signed-word) recip-255)  (read-signed-dword)))
                 (tv (list->vector (map value->pretty-string (list u-raw (- v-raw))))))
            (model-append-texture-coordinate! model tv)))


        ;; normals
        (do ((j 0 (+ j 1)))
            ((= j normals-count) #t)
          (let* ((x-raw (read-signed-byte))
                 (y-raw (read-signed-byte))
                 (z-raw (read-signed-byte))
                 (x (* x-raw recip-127))
                 (y (* y-raw recip-127))
                 (z (* z-raw recip-127))
                 (v (list->vector (map value->pretty-string (list x y z))))
                 )
            (model-append-normal! model v)))

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

        (let loop ((j 0)
                   (rev-new-faces-corners '()))
          (cond ((= j polygons-count)
                 (set! new-faces-corners (reverse rev-new-faces-corners)))
                (else
                   (let* ((face-flags (read-word))
                          (v0 (read-word))
                          (v1 (read-word))
                          (v2 (read-word))
                          ;; same number of normals as vertices, so this works:
                          (face-corner-0 (make-face-corner v0 v0 v0))
                          (face-corner-1 (make-face-corner v1 v1 v1))
                          (face-corner-2 (make-face-corner v2 v2 v2))
                          (face-corners (list face-corner-2 face-corner-1 face-corner-0))
                          ;; (face (mesh-prepend-face! model mesh face-corners material
                          ;;                           vertex-index-start texture-index-start normal-index-start))
                          )
                     (loop (+ j 1) (cons face-corners rev-new-faces-corners))))))


        ;; jump over VertexPiece entries
        (set! i (+ i (* vertex-piece-count 4)))

        ;; PolygonTex entries
        (do ((j 0 (+ j 1)))
            ((= j polygon-tex-count) #t)
          (let* ((mat-face-count (read-word))
                 (texture-index (read-word))
                 (texture-30-index (vector-ref textures texture-index)) ;; a fragment-30
                 (texture-30 (vector-ref frags texture-30-index))
                 (texture-5-index (fragment-30-reference texture-30))
                 (texture-5 (vector-ref frags texture-5-index))
                 (texture-4-index (fragment-5-reference texture-5))
                 (texture-4 (vector-ref frags texture-4-index))
                 (texture-3-index (vector-ref (fragment-4-references texture-4) 0))
                 (texture-3 (vector-ref frags texture-3-index))
                 (texture-filename (car (fragment-3-filenames texture-3)))
                 (material-name (substring (string-downcase texture-filename) 0 (- (string-length texture-filename) 4)))
                 (material (model-get-material-by-name model material-name)))

            (cerr "material -- count: " mat-face-count ", name: " material-name ", material: " material "\n")
            (do ((k 0 (+ k 1)))
                ((= k mat-face-count) #t)
              ;; (face-set-material! (car new-faces-corners) material)
              (let ((face-corners (car new-faces-corners)))
                (mesh-prepend-face! model mesh face-corners material
                                    vertex-index-start texture-index-start normal-index-start))
              (set! new-faces-corners (cdr new-faces-corners)))
            #t))


        (model-prepend-mesh! model mesh)


        #t))


    (define (read-fragment wld-data i frag-index old string-hash model frags)
      (let* ((size (read-dword wld-data i))
             (id (read-dword wld-data (+ i 4)))
             (name-key (read-signed-dword wld-data (+ i 8)))
             (name (hash-table-ref/default string-hash name-key "")))

        ;; (cerr "size = " size ", id = " (number->string id 16) ", name = " name-key " = " name "\n")

        (cond
         ((= id #x03)
          (read-fragment-3-texture-filenames wld-data (+ i 12) frag-index old name model frags))
         ((= id #x04)
          (read-fragment-4-texture wld-data (+ i 12) frag-index old name model frags))
         ((= id #x05)
          (read-fragment-5-texture wld-data (+ i 12) frag-index old name model frags))
         ((= id #x30)
          (read-fragment-30-texture wld-data (+ i 12) frag-index old name model frags))
         ((= id #x31)
          (read-fragment-31-textures wld-data (+ i 12) frag-index old name model frags))
         ((= id #x36)
          (read-fragment-36-mesh-plain wld-data (+ i 12) frag-index old name model frags))
         )

        (+ (+ i size) 8)))


    (define (main-program)
      (define (usage why)
        (cerr why "\n")
        (cerr "eqwld-to-obj [arguments] lines-input-file points-input-file\n")
        (cerr "    --obj                      output an obj file\n")
        (cerr "    --scad                     output an openscad file\n")
        (exit 1))

      (let* ((args (parse-command-line `((--obj)
                                         (--scad)
                                         (-?) (-h))))
             (output-obj #f)
             (output-scad #f)
             (extra-arguments '()))
        (for-each
         (lambda (arg)
           (case (car arg)
             ((-? -h) (usage ""))
             ((--obj)
              (if (or output-obj output-pnm output-scad output-texture)
                  (usage "give only one of: --obj --pnm --scad --texture --caves"))
              (set! output-obj #t))
             ((--scad)
              (if (or output-obj output-pnm output-scad output-texture)
                  (usage "give only one of: --obj --pnm --scad --texture --caves"))
              (set! output-scad #t))
             ((--)
              (set! extra-arguments (cdr arg)))))
         args)

        (if (not (= (length extra-arguments) 1))
            (usage "give wld filename as an argument"))

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
                 (snow-assert false)))
          (step-i)

          (let* ((version (number->string (read-version wld-data (step-i)) 16))
                 (old #t) ;; XXX based on version
                 (fragment-count (read-fragment-count wld-data (step-i)))
                 (header-3 (read-header-3 wld-data (step-i)))
                 (header-4 (number->string (read-header-4 wld-data (step-i)) 16))
                 (string-hash-size (read-string-hash-size wld-data (step-i)))
                 (header-6 (read-header-6 wld-data (step-i)))
                 (string-hash (read-string-hash wld-data i string-hash-size))
                 (model (make-empty-model))
                 (frags (make-vector fragment-count #f))
                 )

            (add-material-library model
                                  (string-append
                                   (substring wld-filename 0 (- (string-length wld-filename) 4))
                                   ".mtl"))

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
                     (set! i (read-fragment wld-data i frag-index old string-hash model frags))
                     (loop (+ frag-index 1)))))

            (write-obj-model model (current-output-port))
            ))

        ;; (cond
        ;;  (output-obj
        ;;   ;; output a model
        ;;   (close-model)
        ;;   (write-obj-model model (current-output-port)))

        ;;  (output-scad
        ;;   ;; output an openscad file
        ;;   (close-model)
        ;;   (write-scad-file
        ;;    (list (model->scad-polyhedron model))
        ;;    (current-output-port)))

        ;;  (else
        ;;   ;; else complain
        ;;   (usage "give only one of: --obj or --scad")))

        ))
    ))
