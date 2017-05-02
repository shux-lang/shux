#!/bin/sh

compiler=./shuxc
runtime=lli
test_ext=".shux"
obj_ext=".ll"
out_ext=".out"
expected_ext=".test"
passes=0
fails=0

if [ $# -eq 1 ]; then
    echo "single test running not yet supported"
    # this should be extended to just run the below with test_names=our one file
else
    echo "running all tests sequentially\n"
    test_names=`ls *.shux`
    for test in $test_names 
    do
    	test=`echo "$test" | cut -d'.' -f1`
    	echo -n "running "$test"... "

        # what intermediate objects do we need?
    	test_obj=$test$obj_ext
    	test_out=$test$out_ext
    	expected_out=$test$expected_ext
    	
        # run compiler and invoke runtime
        # TODO this needs to capture errors if the compiler fails and redirect them to test_out
        $compiler $test$test_ext > $test_obj
    	$runtime $test_obj > $test_out

        if [ ! -f $expected_out ]; then
            # expected output file not found
            echo "expected output file not found!"
            fails=$((fails+1))
            continue
        fi
    	if diff -q $test_out $expected_out > /dev/null; then
    	    echo "passed!"
            passes=$((passes+1))
    	else
    	    echo "failed! please check output files for debug info"
    	    fails=$((fails+1))
        fi
    done
fi
