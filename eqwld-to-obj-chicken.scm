#! /bin/sh
#| -*- scheme -*-
exec csi -include-path /usr/local/share/scheme -s $0 "$@"
|#

(use r7rs)

(include "srfi/60.sld")
(include "snow/assert.sld")
(include "seth/cout.sld")
(include "seth/raster.sld")
(include "seth/image.sld")
(include "seth/pbm.sld")
(include "seth/math-3d.sld")
(include "seth/strings.sld")
(include "foldling/command-line.sld")
(include "eqwld-to-obj-main.sld")

(import (scheme base)
        (eqwld-to-obj-main))
(main-program)
