#!/bin/sh
cd `dirname $0`
erlc -o $PWD/ebin test/erobix_int_tests.erl
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -config elog -s erobix_int_tests
