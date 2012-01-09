set xlabel "Laikas, ms"
set ylabel "Kiek tinkų pamiršo maršrutą, procentais"
set terminal png
set output "$0"
plot "$1" u 1:2 title "" with lp
