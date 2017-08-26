

;; http://www.voronoi.com/wiki/index.php?title=Main_Page


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


    (define (read-fragment-36-mesh-plain wld-data i old name model)
      (let* ((recip-127 (/ 1.0 127.0))
             (recip-255 (/ 1.0 256.0))
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
             (fragment1 (read-dword))
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
             (material ;; (model-get-material-by-name model "leaf_mtl")
              #f)
             (vertex-index-start (coordinates-length (model-vertices model)))
             (texture-index-start (coordinates-length (model-texture-coordinates model)))
             (normal-index-start (coordinates-length (model-normals model)))
             )
        (cerr "---\n")
        (cerr "flags = " (number->string flags 16) "\n")
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

        ;; vertices
        (do ((j 0 (+ j 1)))
            ((= j vertex-count) #t)
          (let* ((x-raw (read-signed-word))
                 (y-raw (read-signed-word))
                 (z-raw (read-signed-word))
                 (x (+ center-x (* x-raw scale)))
                 (y (+ center-y (* y-raw scale)))
                 (z (+ center-z (* z-raw scale)))
                 (v (list->vector (map value->pretty-string (list x y z))))
                 )
            (model-append-vertex! model (make-vertex v (vector 'unset 'unset 'unset)))
            ))

        ;; vertex coords
        (do ((j 0 (+ j 1)))
            ((= j tex-coords-count) #t)
          (let* ((u-raw (if old (read-signed-word) (read-signed-dword)))
                 (v-raw (if old (read-signed-word) (read-signed-dword))))
            #t))

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
            (model-append-normal! model v)
            ))

        ;; vertex colors
        (do ((j 0 (+ j 1)))
            ((= j color-count) #t)
          (let* ((color-raw (read-dword)))
            #t))

        ;; faces
        (do ((j 0 (+ j 1)))
            ((= j polygons-count) #t)
          (let* ((face-flags (read-word))
                 (v0 (read-word))
                 (v1 (read-word))
                 (v2 (read-word))
                 (face-corner-0 (make-face-corner v0 'unset 'unset))
                 (face-corner-1 (make-face-corner v1 'unset 'unset))
                 (face-corner-2 (make-face-corner v2 'unset 'unset))
                 (face-corners (list face-corner-0 face-corner-1 face-corner-2))
                 )
            (cerr "face = " (list v0 v1 v2) "\n")
            (mesh-prepend-face! model mesh face-corners material vertex-index-start texture-index-start normal-index-start)
            #t))


        (model-prepend-mesh! model mesh)


        #t))


    (define (read-fragment wld-data i old string-hash model)
      (let* ((size (read-dword wld-data i))
             (id (read-dword wld-data (+ i 4)))
             (name-key (read-signed-dword wld-data (+ i 8)))
             (name (hash-table-ref/default string-hash name-key "")))

        ;; (cerr "size = " size ", id = " (number->string id 16) ", name = " name-key " = " name "\n")

        (cond ((= id #x36)
               (read-fragment-36-mesh-plain wld-data (+ i 12) old name model))
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
                     (set! i (read-fragment wld-data i old string-hash model))
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
