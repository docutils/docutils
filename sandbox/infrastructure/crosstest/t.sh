#!/bin/sh

# test local wheel
WHL=../docutils-0.17.1b2.dev0-py2.py3-none-any.whl
WHL=TESTPYPI
# PRE =

for VDIR in ./py3.* ; do
    echo "$VDIR"
    echo "=========="

    cd $VDIR
    export PYTHONPATH= ; . bin/activate
    export PYTHONWARNINGS=default
    export LC_ALL=C
    if [ -e $WHL ] ; then
        python -m pip install $WHL
    elif [ $WHL = "TESTPYPI" ] ; then
        python -m pip install --no-cache-dir --index-url https://test.pypi.org/simple/ --no-deps docutils
        python -m pip install --upgrade --index-url https://test.pypi.org/simple/ --no-deps docutils
    else
        python -m pip install $WHL
    fi
    cp -Lr ~/projects/docutils-code/docutils/test . 
    python test/alltests.py
    read X
    deactivate
    cd ..
done

