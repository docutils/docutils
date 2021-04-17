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
        # cache command available in newer pips
        python -m pip cache remove docutils
        python -m pip install --no-cache-dir --index-url https://test.pypi.org/simple/ --no-deps docutils
    else
        python -m pip install $WHL
    fi
    python -m pip show docutils
    read -p "pause" X
    cp -Lr ~/projects/docutils-code/docutils/test . 
    python test/alltests.py
    read -p "pause" X
    deactivate
    cd ..
done

