
set term postscript eps enhanced color
set output "figs/census-perf.eps"
set key left nobox
set size 0.5,0.5
set autoscale
unset log
unset label
unset title
unset xtic

set xlabel "Number of Records (millions)"
set ylabel "Wall Clock (seconds)"
#set title "Cloud Random Rendering Peformance for Census Data (100 frames)" 

#set key 0.01,100
#set xtics (25 0, 50 1, 75 2, 100 3, 125 4)
set xtic auto
set ytic auto
set xr[25:125]
plot "census-perf.txt" using 1:2 title '5 workers' with linespoints, \
"census-perf.txt" using 1:3 title '10 workers' with linespoints, \
"census-perf.txt" using 1:4 title '15 workers' with linespoints


set output "figs/cost-part.eps"
#set key autotitle columnheader
set style histogram rowstacked
set style data histograms
set yr[0:50]
set auto x
set boxwidth 0.5
#set style fill 
unset xlabel
set key right
set xtics nomirror rotate by -45 scale 0
set xtics ("Census-H" 0, "Census-L" 1, "KDDCup-H" 2, "KDDCup-L" 3)
plot 'census-perf3'  using 3 t "Transfer", '' using 4 t "Compression"
set xtics nomirror norotate

set output "figs/cost-resol.eps"
set style histogram cluster gap 1 title  offset character 0, 0, 0
set style data histograms
#set style fill solid 0.5 border lt -1
set boxwidth 0.5
set yr[0:300]
unset xlabel
set key left
set xtics ("Census" 0, "KDDCup" 1)
plot 'census-perf4'  using 2 t "High Res", '' using 3 t "Low Res" 
