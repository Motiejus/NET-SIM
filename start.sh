#!/bin/bash

run_sim() {
    DIR=$1
    erl -config priv/silent_run.config -pz ebin/ -pz deps/*/ebin/ -boot start_sasl -s netsim start_app \
        ${DIR}/nodelist.txt ${DIR}/channels.txt ${DIR}/simulation.txt ${DIR}/settings.txt \
        ${DIR}/ticks.txt ${DIR}/total_traffic.txt ${DIR}/traffic.txt \
        -s init stop -noshell -noinput || exit 1
}

echo -n "Compiling... "
make -s || exit 1
echo "done"

if [[ "$1" == "priv" ]]; then
    WORKDIR="priv"
else
    WORKDIR=res/*
    echo -n "Generating graphs..."
    ./generator/graph.py || exit 1
    echo "done"
fi

for dir in $WORKDIR; do
    echo -n "Running simulation in $dir... "
    run_sim $dir
    echo "done"
    echo -n "Drawing plots ... "
    gnuplot -e " call \"generator/ticks.gp\" \"${dir}/ticks.svg\" \"${dir}/ticks.txt\"" || exit 1
    gnuplot -e " call \"generator/total_traffic.gp\" \"${dir}/total_traffic.svg\" \"${dir}/total_traffic.txt\"" || exit 1
    echo -n "done"
    echo "Plots written to ${dir}/ticks.svg, ${dir}/total_traffic.svg"
done
