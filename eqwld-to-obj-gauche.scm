#! /bin/bash
#| -*- scheme -*-
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
SOURCE="$(readlink "$SOURCE")"
[[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
exec gosh \
-e '(set! *load-suffixes* (cons ".sld" *load-suffixes*))' \
-e "(append! *load-path* (list \"$DIR\" \".\"))" \
-r7 "$0" "$@"
|#


(import (scheme base)
        (eqwld-to-obj-main))
(main-program)
