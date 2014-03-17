#!/bin/sh
#
# prep: prep analytics directory structure
#
# usage: prep DIRNAME
#

if [ "$#" == "0" ]; then
    echo "ERROR: Please specify a directory name"
    echo 
    echo "USAGE: prep DIRNAME"
    exit
fi
   

DIR=$1

if [ ! -d "${DIR}" ]; then
    mkdir -p ${DIR}/R \
             ${DIR}/data \
             ${DIR}/docs \
             ${DIR}/output \
             ${DIR}/python \
             ${DIR}/support \
             ${DIR}/tmp
    > ${DIR}/readme.md
    ls -lR ${DIR}
else
    echo "Directory "${DIR}" already exists"
fi

