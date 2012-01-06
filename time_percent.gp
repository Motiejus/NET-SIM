set xlabel "Time, ms"
set ylabel "Percentage of nodes that got the route"
set terminal png
set output "$0"
plot "$1" u 1:2 title "" with lp
