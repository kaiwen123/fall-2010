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
set key 28,16
set size 0.8,0.8
set output "figs/adult.5d.blocks.eps"
set title "Size vs. #Blocks (RTree/Linear)"
plot "adult.5d.final" using 1:(log($4)) title 'RTree Original' with linespoints, \
    "adult.5d.final" using 1:(log($12)) title 'Linear Original' with linespoints, \
    "adult.5d.final" using 1:(log($18)) title 'RTree Transformed' with linespoints, \
    "adult.5d.final" using 1:(log($24)) title 'Linear Transformed' with linespoints

set xlabel "Number of Records (thousands)"
set ylabel "Wall Clock (seconds)"
set output "figs/adult.5d.time.eps"
set key default
set key 40,680
set title "Size vs. #Time (RTree)"
set style line 2 lt 1 pt 7
plot "adult.5d.final" using 1:2:3 title 'RTree Original' w yerr, \
    "adult.5d.final" using 1:2 notitle w linespoints, \
    "adult.5d.final" using 1:8:9 title 'Linear Original' w yerr, \
    "adult.5d.final" using 1:8 notitle w linespoints, \
    "adult.5d.final" using 1:14:15 title 'RTree Transformed' w yerr, \
    "adult.5d.final" using 1:14 notitle w linespoints, \
    "adult.5d.final" using 1:20:21 title 'Linear Transformed' w yerr, \
    "adult.5d.final" using 1:20 notitle with linespoints
    
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set xlabel "Dimension"
set ylabel "Number of blocks (log)"
#set title "Cloud Random Rendering Peformance for Census Data (100 frames)" 
set output "figs/adult.k20.blocks.eps"
set key right top nobox
set key 9,17
set title "Dimension vs. #Blocks (RTree/Linear)"
plot "adult.k20.final" using 1:(log($4)) title 'RTree Original' with linespoints, \
    "adult.k20.final" using 1:(log($12)) title 'Linear Original' with linespoints, \
    "adult.k20.final" using 1:(log($18)) title 'RTree Transformed' with linespoints, \
    "adult.k20.final" using 1:(log($24)) title 'Linear Transformed' with linespoints

#"50k-0.5-blocks.txt" using 1:(log($5)) title 'Exhaustive Transformed' with linespoints

set xlabel "Dimension"
set ylabel "Wall Clock (seconds)"
set key default
set key left top nobox
set output "figs/adult.k20.time.eps"
set title "Dimension vs. Time (RTree)"
plot "adult.k20.final" using 1:2:3 title 'RTree Original' w yerr, \
    "adult.k20.final" using 1:2 notitle w linespoints, \
    "adult.k20.final" using 1:8:9 title 'Linear Original' w yerr, \
    "adult.k20.final" using 1:8 notitle w linespoints, \
    "adult.k20.final" using 1:14:15 title 'RTree Transformed' w yerr, \
    "adult.k20.final" using 1:14 notitle w linespoints, \
    "adult.k20.final" using 1:20:21 title 'Linear Transformed' w yerr, \
    "adult.k20.final" using 1:20 notitle with linespoints

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set xlabel "Length of Query"
set ylabel "Number of blocks (log)"
set key right center nobox
set key 0.9,16
set output "figs/norm.5d.blocks.eps"
set title "Distance vs. #Blocks(RTree/Linear)"
#set title "Cloud Random Rendering Peformance for Census Data (100 frames)" 

plot "norm.5d.final" using 1:(log($4)) title 'RTree Original' with linespoints, \
    "norm.5d.final" using 1:(log($12)) title 'Linear Original' with linespoints, \
    "norm.5d.final" using 1:(log($18)) title 'RTree Transformed' with linespoints, \
    "norm.5d.final" using 1:(log($24)) title 'Linear Transformed' with linespoints

set xlabel "Number of Records (thousands)"
set ylabel "Wall Clock (seconds)"
set output "figs/norm.5d.time.eps"
set key default
set key 30,280
set title "Size vs. Time (RTree)"
set style line 2 lt 1 pt 7
plot "norm.5d.final" using 1:2:3 title 'RTree Original' w yerr, \
    "norm.5d.final" using 1:2 notitle w linespoints, \
    "norm.5d.final" using 1:8:9 title 'Linear Original' w yerr, \
    "norm.5d.final" using 1:8 notitle w linespoints, \
    "norm.5d.final" using 1:14:15 title 'RTree Transformed' w yerr, \
    "norm.5d.final" using 1:14 notitle w linespoints, \
    "norm.5d.final" using 1:20:21 title 'Linear Transformed' w yerr, \
    "norm.5d.final" using 1:20 notitle with linespoints
    
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set xlabel "Dimension"
set ylabel "Number of blocks (log)"
#set title "Cloud Random Rendering Peformance for Census Data (100 frames)" 
set output "figs/norm.k20.blocks.eps"
set key right top nobox
set key 9,8
set title "Dimension vs. #Blocks (RTree/Linear)"
plot "norm.k20.final" using 1:(log($4)) title 'RTree Original' with linespoints, \
    "norm.k20.final" using 1:(log($12)) title 'Linear Original' with linespoints, \
    "norm.k20.final" using 1:(log($18)) title 'RTree Transformed' with linespoints, \
    "norm.k20.final" using 1:(log($24)) title 'Linear Transformed' with linespoints

#"50k-0.5-blocks.txt" using 1:(log($5)) title 'Exhaustive Transformed' with linespoints

set xlabel "Dimension"
set ylabel "Wall Clock (seconds)"
set key default
set key left top nobox
set output "figs/norm.k20.time.eps"
set title "Dimension vs. Time (RTree)"
plot "norm.k20.final" using 1:2:3 title 'RTree Original' w yerr, \
    "norm.k20.final" using 1:2 notitle w linespoints, \
    "norm.k20.final" using 1:8:9 title 'Linear Original' w yerr, \
    "norm.k20.final" using 1:8 notitle w linespoints, \
    "norm.k20.final" using 1:14:15 title 'RTree Transformed' w yerr, \
    "norm.k20.final" using 1:14 notitle w linespoints, \
    "norm.k20.final" using 1:20:21 title 'Linear Transformed' w yerr, \
    "norm.k20.final" using 1:20 notitle with linespoints
