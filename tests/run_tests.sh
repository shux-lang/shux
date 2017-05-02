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
        ERROR="$($compiler $test$test_ext 2>&1 > $test_obj)"
        if [ -n "$ERROR" ]; then    
            echo $ERROR > $test$out_ext
            if echo $ERROR | grep -q "Uncaught exception"; then
                echo "compilation failed! saving $test$out_ext ❌"
                fails=$((fails+1))
                continue
            fi
        fi

        # check for valid test file
        if [ ! -f $expected_out ]; then
            # expected output file not found
            echo "expected output file not found! ❌"
            fails=$((fails+1))
            continue
        fi

        # check type of test
        expected_head=$(head -n 1 $expected_out)
        expected_body=/tmp/expected_body
        echo -n $(tail -n +2 $expected_out) > $expected_body
        if [ "$expected_head" = "PASS" ]; then
            # diff against expected output
            $runtime $test_obj > $test_out 2>&1
            if diff -q $test_out $expected_body > /dev/null; then
                echo "passed! ✅"
                passes=$((passes+1))
            else
                echo "failed! please check $test$out_ext for debug info ❌"
                fails=$((fails+1))
            fi
        elif [ "$expected_head" = "FAIL" ]; then
            # pattern match against execption keywords
            echo $(diff -q $test_out $expected_body)
            if diff -q $test_out $expected_body > /dev/null; then
                echo "passed! ✅"
                passes=$((passes+1))
            else
                echo "failed! please check $test$out_ext for debug info ❌"
                fails=$((fails+1))
            fi
        else
            echo "invalid test specification ❌"
            fails=$((fails+1))
            continue
        fi
    done

    echo "\n$((fails+passes)) tests run", "$fails fail(s)", "$passes passes"
    if [ $fails -eq 0 ]; then
        echo "all tests passed. you done it 💯"
    fi
fi
