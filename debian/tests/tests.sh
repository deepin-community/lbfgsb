#!/bin/bash

names="test1 test2"

oneTimeSetUp() {
    set -u
    cp debian/tests/test*.f90 $AUTOPKGTEST_TMP
}

testSO() {
    cwd=$(pwd)
    cd $AUTOPKGTEST_TMP
    for name in $names
    do
        echo "Building."
        set -x
        gfortran -O2 -o $name $name.f90 -llbfgsb
        ret=$?
        set +x
        assertEquals 0 $ret
        
        echo "Running."
        ./$name
        ret=$?
        assertEquals 0 $ret
    done
    cd $cwd
}

testStatic() {
    cwd=$(pwd)
    cd $AUTOPKGTEST_TMP
    for name in $names
    do
        echo "Building (statically linked)."
        set -x
        gfortran $name.f90 -O2 -static -llbfgsb -llapack -lblas -lgfortran -o $name-static
        ret=$?
        set +x
        assertEquals 0 $ret
        
        echo "Running (statically linked)."
        ./$name-static
        ret=$?
        assertEquals 0 $ret
    done
    cd $cwd
}

. shunit2
