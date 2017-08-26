#! /bin/bash
#| -*- scheme -*-
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
SOURCE="$(readlink "$SOURCE")"
[[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
done
DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
X=$CHIBI_MODULE_PATH
CHIBI_MODULE_PATH="" exec chibi-scheme -A "$DIR" -A "$X" -A . -s "$0" "$@"
|#

(import (scheme base)
        (eqwld-to-obj))
(main-program)
