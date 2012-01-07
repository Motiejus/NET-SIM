#!/bin/sh

./rebar compile
erl -config priv/silent_run.config -pa ebin/ -pa deps/*/ebin/ -boot start_sasl -s netsim start_app -s init stop -noshell -noinput
echo -n "Drawing plot ... "
gnuplot -e ' call "time_percent.gp" "res/output.png" "res/output.txt"'
echo "done"
echo "Plot written to res/output.png"
