#!/bin/sh
#
# test manpages with lexgrog for whatis/apropos

DIR=output

CNT=0
ERR=0
FAILS=""
for F in $DIR/*.man ; do 
    CNT=$(( CNT + 1 ))
    lexgrog $F
    if test $? -ne 0 ; then
        ERR=$(( ERR + 1 ))
        FAILS="$FAILS $(basename $F)"
    fi
done
if test $ERR -eq 0 ; then
    echo "OK $CNT tests passed"
else
    echo "ERROR $ERR of $CNT tests failed"
    echo "-- $FAILS"
fi 
