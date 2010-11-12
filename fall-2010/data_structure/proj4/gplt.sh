#!/usr/bin/gnuplot
#set term postscript eps enhanced #color
set terminal postscript enhanced #color
set size 1,1
set autoscale
set xlabel "Alpha"
set ylabel "Average Number of Probes"
set output "alpha-collision.ps"

set title "Alpha VS. Probes for Two Closed Hashing Methods"
plot "double.txt" using 1:2 title 'Double Probing' with linespoints, \
"random.txt" using 1:2 title "Random Probing" with linespoints
#set title "Alpha VS. Collisions for Insert/Search Random Closed Hashing"
#set output "insert-alpha-collision-mean.eps"
#plot "insert-collision-mean.txt" using 1:2 title "mean" with linespoints

