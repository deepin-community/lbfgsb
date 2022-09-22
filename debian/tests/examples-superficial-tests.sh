#!/bin/bash

names="/usr/bin/lbfgsb-examples_driver1_77 \
       /usr/bin/lbfgsb-examples_driver1_90 \
       /usr/bin/lbfgsb-examples_driver2_77 \
       /usr/bin/lbfgsb-examples_driver2_90 \
       /usr/bin/lbfgsb-examples_driver3_77 \
       /usr/bin/lbfgsb-examples_driver3_90"

testExamplesSuperficially() {
    set -u

    cd $AUTOPKGTEST_TMP
    
    for name in $names
    do
        set -x
        $name
        ret=$?
        set +x
        assertEquals 0 $ret
    done
}

. shunit2
