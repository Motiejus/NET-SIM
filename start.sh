#!/bin/bash

run_sim() {
    DIR=$1
    erl -config priv/silent_run.config -pz ebin/ -pz deps/*/ebin/ -boot start_sasl -s netsim start_app \
        ${DIR}/nodelist.txt ${DIR}/channels.txt ${DIR}/simulation.txt ${DIR}/settings.txt \
        ${DIR}/ticks.txt ${DIR}/total_traffic ${DIR}/traffic ${DIR}/traffic_histogram \
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

    gnuplot -e " call \"generator/announce.gp\" \"${dir}/ticks_announce.png\" \"${dir}/ticks.txt_1.txt\"" || exit 1
    gnuplot -e " call \"generator/denounce.gp\" \"${dir}/ticks_denounce.png\" \"${dir}/ticks.txt_2.txt\"" || exit 1
    gnuplot -e " call \"generator/total_traffic.gp\" \"${dir}/total_traffic.png\" \"${dir}/total_traffic.txt\"" || exit 1
    gnuplot -e " call \"generator/traffic_histogram.gp\" \"${dir}/traffic_histogram_announce.png\" \"${dir}/traffic_histogram_1.txt\"" || exit 1
    gnuplot -e " call \"generator/traffic_histogram.gp\" \"${dir}/traffic_histogram_announce.png\" \"${dir}/traffic_histogram_2.txt\"" || exit 1

    echo -n "done"
    echo "Plots written to ${dir}/ticks.svg, ${dir}/total_traffic.svg, ${dir}/traffic_histogram.svg"
done
