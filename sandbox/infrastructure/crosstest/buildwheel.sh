#!/bin/sh

# build wheel in each virtual environment

PATH2SRC=$(pwd)/../../../docutils

TESTDIR=$(pwd)

# python2.7 -m virtualenv py2.7
for VDIR in ./py3.* ; do
    echo "$VDIR"
    echo "=========="

    cd $VDIR
    export PYTHONPATH= ; . bin/activate
    cd $PATH2SRC
    find . -type f -name "*.pyc" -delete
    rm -r dist
    rm -r build
    export PYTHONWARNINGS=default
    python setup.py bdist_wheel --universal
    deactivate
    cd $TESTDIR
    mv $PATH2SRC/dist $VDIR
done

