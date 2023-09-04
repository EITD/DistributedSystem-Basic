set term postscript eps enhanced color
set output "results.eps"

set title "Rudy Vs RudyImproved"
set xlabel "Request Numbers"
set ylabel "Response Time"

set xrange [90:110]
set yrange [3770000:4640000]

plot "experiment.dat" using 1:2 axis x1y1 with linespoints title "Rudy",\
     "experiment.dat" using 1:3 axis x1y1 with linespoints title "Improved Rudy"

