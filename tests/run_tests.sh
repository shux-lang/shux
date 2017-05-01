#!/bin/sh

compiler=./shuxc
runtime=lli
test_ext=".shux"
obj_ext=".ll"
out_ext=".out"

if [ $# -eq 1 ]; then
    echo "Single test running not yet supported"
else
    echo "Running all tests sequentially..."
    test_names=`ls *.shux`
    for test in $test_names
    do
	test=`echo "$test" | cut -d'.' -f1`
	echo 'running '$test
	test_obj=$test$obj_ext
	test_out=$test$out_ext
	$compiler $test$test_ext > $test_obj
	$runtime $test_obj > $test_out
    done
fi
