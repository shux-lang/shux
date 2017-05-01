#!/bin/sh

compiler=./shuxc
runtime=lli
obj_ext=".ll"
out_ext=".out"

echo "Running all tests sequentially..."
test_names=`ls *.shux`
for test in $test_names
do
    echo 'running '$test
    test_obj=$test$obj_ext
    test_out=$test$out_ext
    $compiler $test > $test_obj
    $runtime $test_obj > $test_out 
done
