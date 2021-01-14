#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
LIB=${HOME}/z/haskell-lts-16.26

mkdir -p ${LIB}

stack --silent unpack base --to ${LIB}
stack --silent unpack containers --to ${LIB}
stack --silent unpack directory --to ${LIB}
stack --silent unpack filepath --to ${LIB}
stack --silent unpack fgl --to ${LIB}
stack --silent unpack mtl --to ${LIB}
stack --silent unpack reducers --to ${LIB}
stack --silent unpack temporary --to ${LIB}
stack --silent unpack text --to ${LIB}
stack --silent unpack transformers --to ${LIB}
stack --silent unpack typed-process --to ${LIB}
stack --silent unpack xml-conduit --to ${LIB}
stack --silent unpack xml-types --to ${LIB}

if [ -f ${DIR}/tags ]; then rm ${DIR}/tags; fi
hasktags -L --ctags ${DIR}/src/ ${LIB}
