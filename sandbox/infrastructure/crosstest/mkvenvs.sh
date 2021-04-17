#!/bin/sh

# make virtual environments

# python2.7 -m virtualenv py2.7
for I in 5 6 7 8 9 10 ; do
    python3.$I -m venv py3.$I
done

