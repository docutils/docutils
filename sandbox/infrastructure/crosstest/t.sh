#!/bin/sh

# test 
WHL=../docutils-0.17.1b2.dev0-py2.py3-none-any.whl

for VDIR in ./py3.* ; do
    echo "$VDIR"
    echo "=========="

    cd $VDIR
    export PYTHONPATH= ; . bin/activate
    export PYTHONWARNINGS=default
    export LC_ALL=C
    python -m pip install $WHL
    cp -Lr ~/projects/docutils-code/docutils/test . 
    python test/alltests.py
    read X
    deactivate
    cd ..
done

