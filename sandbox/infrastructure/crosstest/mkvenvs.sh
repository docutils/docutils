#!/bin/sh

# make virtual environments

# python2.7 -m virtualenv py2.7
for I in 5 6 7 8 9 10 ; do
    VDIR=py3.$I
    python3.$I -m venv $VDIR
    cd $VDIR
    . bin/activate
    python -m pip install setuptools wheel
    deactivate
    cd ..
done

