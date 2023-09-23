set term postscript eps enhanced color
set output "results.eps"

set title "Lamport Vs Vector"
set xlabel "Messages Received"
set ylabel "Messages In Queue"

set xrange [10:70]
set yrange [0:35]

plot "experiment.dat" using 1:2 axis x1y1 with linespoints title "Lamport",\
     "experiment.dat" using 1:3 axis x1y1 with linespoints title "Vector"

