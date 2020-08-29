#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

mkdir -p ${DIR}/.lib

stack --silent unpack base --to ${DIR}/.lib
stack --silent unpack containers --to ${DIR}/.lib
stack --silent unpack directory --to ${DIR}/.lib
stack --silent unpack filepath --to ${DIR}/.lib
stack --silent unpack fgl --to ${DIR}/.lib
stack --silent unpack mtl --to ${DIR}/.lib
stack --silent unpack reducers --to ${DIR}/.lib
stack --silent unpack temporary --to ${DIR}/.lib
stack --silent unpack text --to ${DIR}/.lib
stack --silent unpack transformers --to ${DIR}/.lib
stack --silent unpack typed-process --to ${DIR}/.lib
stack --silent unpack xml-conduit --to ${DIR}/.lib
stack --silent unpack xml-types --to ${DIR}/.lib

if [ -f ${DIR}/tags ]; then rm ${DIR}/tags; fi
hasktags -L --ctags ${DIR}/.lib/ ${DIR}/src/
