set xlabel "Perduotos informacijos kiekis, KB"
set ylabel "Dažnis"
set terminal png
set output "$0"
plot "$1" u ($$1/1024) :2 notitle smooth freq w boxes fs solid 0.7 lc rgb"green"
