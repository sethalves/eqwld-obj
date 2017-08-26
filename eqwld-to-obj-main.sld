

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
          )
  (begin

    (define (read-dword wld-data i)
      (+ (arithmetic-shift (bytevector-u8-ref wld-data (+ i 3)) 24)
         (arithmetic-shift (bytevector-u8-ref wld-data (+ i 2)) 16)
         (arithmetic-shift (bytevector-u8-ref wld-data (+ i 1)) 8)
         (bytevector-u8-ref wld-data i)))


    ;; (define (read-magic wld-data i)
    ;;   ;; Magic : DWORD
    ;;   ;; This always contains 0x54503D02. It identifies the file as a .WLD file.
    ;;   (cond ((or (not (= (bytevector-u8-ref wld-data (+ i 3)) #x54))
    ;;              (not (= (bytevector-u8-ref wld-data (+ i 2)) #x50))
    ;;              (not (= (bytevector-u8-ref wld-data (+ i 1)) #x3D))
    ;;              (not (= (bytevector-u8-ref wld-data (+ i 0)) #x02)))
    ;;          (cerr "bad magic\n")
    ;;          (cerr (number->string (bytevector-u8-ref wld-data (+ i 3)) 16) "\n")
    ;;          (cerr (number->string (bytevector-u8-ref wld-data (+ i 2)) 16) "\n")
    ;;          (cerr (number->string (bytevector-u8-ref wld-data (+ i 1)) 16) "\n")
    ;;          (cerr (number->string (bytevector-u8-ref wld-data (+ i 0)) 16) "\n")
    ;;          (snow-assert #f))))

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


    ;; (define (read-string-from-hash wld-data string-start-index)
    ;;   (let loop ((result '())
    ;;              (j string-start-index))
    ;;     (cond ((= (bytevector-u8-ref wld-data j) 0)
    ;;            ;; done
    ;;            (cerr "string: " (list->string (reverse result)) "\n")
    ;;            (+ j 1))
    ;;           (else
    ;;            (let* ((xor-value (vector-ref string-xor-values (modulo (- j string-start-index) 8)))
    ;;                   (encoded-value (bytevector-u8-ref wld-data j))
    ;;                   (decoded-value (bitwise-xor encoded-value xor-value)))
    ;;              (loop (cons (integer->char decoded-value) result)
    ;;                    (+ j 1)))))))


    (define (read-string-from-hash wld-data string-start-index hash-start-index string-hash)
      (let loop ((result '())
                 (j string-start-index))
        (let* ((xor-value (vector-ref string-xor-values (modulo (- j hash-start-index) 8)))
               (encoded-value (bytevector-u8-ref wld-data j))
               (decoded-value (bitwise-xor encoded-value xor-value)))

          ;; (cerr "HERE " (- j hash-start-index) " : "
          ;;       (number->string encoded-value 16) " xor "
          ;;       (number->string xor-value 16) " --> "
          ;;       decoded-value " = " (integer->char decoded-value)
          ;;       "\n")

          (cond ((= decoded-value 0)
                 ;; done
                 (let ((key (- (- hash-start-index j) 1)))
                   ;; (cerr "string at " key ": " (list->string (reverse result)) "\n")
                   (hash-table-set! string-hash key (list->string (reverse result))))
                 (+ j 1))
                (else
                 (loop (cons (integer->char decoded-value) result)
                       (+ j 1)))))))

    (define (read-string-hash wld-data i string-hash-size)
      (let ((string-hash (make-hash-table)))
        (let loop ((j i))
          (cond ((>= j (+ i string-hash-size))
                 ;; done
                 string-hash)
                (else
                 (loop (read-string-from-hash wld-data j i string-hash)))))))


    (define (read-fragment-36-mesh-plain wld-data size name)
      #t)


    (define (read-fragment wld-data i string-hash)
      (let* ((size (read-dword wld-data i))
             (id (read-dword wld-data (+ i 4)))
             (name-ref (read-dword wld-data (+ i 8)))
             (name-key (- (+ (bitwise-xor name-ref #xffffffff) 1)))
             (name (hash-table-ref/default string-hash name-key "")))

        (cerr "size = " size ", id = " (number->string id 16) ", name = " name-key " = " name "\n")

        (cond ((= id 36)
               (read-fragment-36-mesh-plain wld-data size name)))

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
                 (fragment-count (read-fragment-count wld-data (step-i)))
                 (header-3 (read-header-3 wld-data (step-i)))
                 (header-4 (number->string (read-header-4 wld-data (step-i)) 16))
                 (string-hash-size (read-string-hash-size wld-data (step-i)))
                 (header-6 (read-header-6 wld-data (step-i)))
                 (string-hash (read-string-hash wld-data i string-hash-size)))
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
                     (set! i (read-fragment wld-data i string-hash))
                     (loop (+ frag-index 1)))))))

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
