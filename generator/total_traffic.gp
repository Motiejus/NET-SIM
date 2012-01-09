set xlabel "Time, ms"
set ylabel "KB sent over network"
set terminal png
set output "$0"
plot "$1" u 1:(($$2 ++ $$3)/1024) title "" with l
