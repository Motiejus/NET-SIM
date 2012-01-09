set xlabel "Laikas, ms"
set ylabel "Kiek tinklų žino maršrutą, procentais"
set terminal png
set output "$0"
plot "$1" u 1:2 title "" with lp
