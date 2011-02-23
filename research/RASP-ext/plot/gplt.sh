#!/usr/bin/gnuplot
set term postscript eps enhanced #color
set size 0.5,0.5
set autoscale
unset log
unset label
unset title
unset xtic

set xlabel "Number of Records (thousands)"
set ylabel "Number of blocks (log)"
#set title "Cloud Random Rendering Peformance for Census Data (100 frames)" 
set output "figs/5d-0.5-blocks.eps"
#set key 0.01,100
#set xtics (25 0, 50 1, 75 2, 100 3, 125 4)
set xtic auto
set ytic auto
set autoscale
#
# set logscale y
# set xtics (10 0, 20 1, 30 2, 40 3, 50 4)
#set xr[25:125]
set key right top nobox
set key 50,17
set title "Size vs. #Blocks (RTree/Linear)"
plot "5d-0.5-blocks.txt" using 1:(log($2)) title 'RTree Original' with linespoints, \
"5d-0.5-blocks.txt" using 1:(log($4)) title 'RTree Transformed' with linespoints,\
"5d-0.5-blocks.txt" using 1:(log($3)) title 'Linear Scan' with linespoints

#"5d-0.5-blocks.txt" using 1:(log($5)) title 'Exhaustive Transformed' with linespoints

set xlabel "Number of Records (thousands)"
set ylabel "Wall Clock (seconds)"
set output "figs/5d-0.5-time.eps"
set key default
set key left top nobox
set title "Size vs. #Time (RTree)"
plot "5d-0.5-time.txt" using 1:2 title 'RTree Original' with linespoints, \
"5d-0.5-time.txt" using 1:4 title 'RTree Transformed' with linespoints

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set xlabel "Dimension"
set ylabel "Number of blocks (log)"
#set title "Cloud Random Rendering Peformance for Census Data (100 frames)" 
set output "figs/50k-0.5-blocks.eps"
set key right top nobox
set key 9,17
set title "Dimension vs. #Blocks (RTree/Linear)"
plot "50k-0.5-blocks.txt" using 1:(log($2)) title 'RTree Original' with linespoints, \
"50k-0.5-blocks.txt" using 1:(log($4)) title 'RTree Transformed' with linespoints,\
"50k-0.5-blocks.txt" using 1:(log($3)) title 'Linear Scan' with linespoints

#"50k-0.5-blocks.txt" using 1:(log($5)) title 'Exhaustive Transformed' with linespoints

set xlabel "Dimension"
set ylabel "Wall Clock (seconds)"
set key default
set key left top nobox
set output "figs/50k-0.5-time.eps"
set title "Dimension vs. Time (RTree)"
plot "50k-0.5-time.txt" using 1:2 title 'RTree Original' with linespoints, \
"50k-0.5-time.txt" using 1:4 title 'RTree Transformed' with linespoints

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set xlabel "Length of Query"
set ylabel "Number of blocks (log)"
set key right center nobox
set key 0.9,16
set output "figs/50k-5d-blocks.eps"
set title "Distance vs. #Blocks(RTree/Linear)"
#set title "Cloud Random Rendering Peformance for Census Data (100 frames)" 

plot "50k-5d-blocks.txt" using 1:(log($2)) title 'RTree Original' with linespoints, \
"50k-5d-blocks.txt" using 1:(log($4)) title 'RTree Transformed' with linespoints,\
"50k-5d-blocks.txt" using 1:(log($3)) title 'Linear Scan' with linespoints

# "50k-5d-blocks.txt" using 1:(log($5)) title 'Exhaustive Transformed' with linespoints

set xlabel "Length of Query"
set ylabel "Wall Clock (seconds)"
set key default
set key left top nobox
set output "figs/50k-5d-time.eps"
set title "Distance vs. Time (RTree)"
plot "50k-5d-time.txt" using 1:2 title 'RTree Original' with linespoints, \
"50k-5d-time.txt" using 1:4 title 'RTree Transformed' with linespoints

# Adult data result - Dimension; 

set xlabel "Dimension"
set ylabel "Wall Clock (seconds)"
set key default
set key 10,23
set output "figs/adult-k20-0.5-time.eps"
set title "Dimension vs. Time (RTree)"
plot "adult-k20-0.5.txt" using 1:6 title 'RTree Original' with linespoints, \
"adult-k20-0.5.txt" using 1:8 title 'RTree Transformed' with linespoints

set xlabel "Dimension"
set ylabel "Number of blocks (log)"
set key right center nobox
set key 9.5,14
set output "figs/adult-k20-0.5-blocks.eps"
set title "Dimension vs. #Blocks(RTree/Linear)"
plot "adult-k20-0.5.txt" using 1:(log($2)) title 'RTree Original' with linespoints, \
"adult-k20-0.5.txt" using 1:(log($4)) title 'RTree Transformed' with linespoints,\
"adult-k20-0.5.txt" using 1:(log($3)) title 'Linear Scan' with linespoints

# Adult data result - Size

set xlabel "Number of Records"
set ylabel "Wall Clock (seconds)"
set key default
set key 45, 8
set output "figs/adult-d5-0.5-time.eps"
set title "Size vs. Time (RTree)"
plot "adult-d5-0.5.txt" using 1:6 title 'RTree Original' with linespoints, \
"adult-d5-0.5.txt" using 1:8 title 'RTree Transformed' with linespoints

set xlabel "Number of Records"
set ylabel "Number of blocks (log)"
set key right center nobox
set key 50,15
set output "figs/adult-d5-0.5.eps"
set title "Size vs. #Blocks(RTree/Linear)"
plot "adult-d5-0.5.txt" using 1:(log($2)) title 'RTree Original' with linespoints, \
"adult-d5-0.5.txt" using 1:(log($4)) title 'RTree Transformed' with linespoints,\
"adult-d5-0.5.txt" using 1:(log($3)) title 'Linear Scan' with linespoints