#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
LIB=${HOME}/.lib/haskell

mkdir -p ${LIB}

stack --silent unpack base --to ${LIB}
stack --silent unpack bytestring --to ${LIB}
stack --silent unpack containers --to ${LIB}
stack --silent unpack directory --to ${LIB}
stack --silent unpack filepath --to ${LIB}
stack --silent unpack process --to ${LIB}
stack --silent unpack mtl --to ${LIB}
stack --silent unpack optparse-applicative --to ${LIB}
stack --silent unpack safe --to ${LIB}
stack --silent unpack text --to ${LIB}
stack --silent unpack transformers --to ${LIB}
stack --silent unpack typed-process --to ${LIB}
stack --silent unpack unordered-containers --to ${LIB}
stack --silent unpack vector --to ${LIB}

if [ -f ${DIR}/tags ]; then rm ${DIR}/tags; fi
hasktags -L --ctags ${DIR}/src/ ${LIB}
