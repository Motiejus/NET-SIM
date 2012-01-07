#!/bin/sh

./rebar compile || exit 1
erl -config priv/silent_run.config -pa ebin/ -pa deps/*/ebin/ -boot start_sasl -s netsim start_app -s init stop -noshell -noinput || exit 1
echo -n "Drawing plot ... "
gnuplot -e ' call "generator/time_percent.gp" "res/output.png" "res/output.txt"' || exit 1
echo "done"
echo "Plot written to res/output.png"
