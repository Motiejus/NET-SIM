#!/bin/sh

run_sim() {
    DIR=$1
    erl -config priv/silent_run.config -pz ebin/ -pz deps/*/ebin/ -boot start_sasl -s netsim start_app \
        ${DIR}/nodelist.txt ${DIR}/channels.txt ${DIR}/simulation.txt ${DIR}/settings.txt \
        ${DIR}/ticks.txt ${DIR}/total_traffic.txt ${DIR}/traffic.txt \
        -s init stop -noshell -noinput || exit 1
}

./generator/graph.py || exit 1
./rebar compile || exit 1
for dir in res/*; do
    run_sim $dir
done

#echo -n "Drawing plot ... "
#gnuplot -e ' call "generator/time_percent.gp" "res/output.png" "res/output.txt"' || exit 1
#echo "done"
#echo "Plot written to res/output.png"
