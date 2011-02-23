#!/bin/sh
# This is the experiment scheduler. 
# With this file, we can automatically deal with a group of
# experiments. 

DIMS="2 3 4 5 6 7 8 9 10"		# Dimensions of data;
DSIZES="10 20 30 40 50"		# Data item numbers; 
DIS="0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9"

for dim in $DIMS
do
    for dsize in $DSIZES
    do
	fname=data.k${dsize}.d${dim}
	size=$(($dsize * 1000))

	# Generate random data; 
	echo 
	echo "*******************************************"

	python generatd.py $dim $size > $fname
	
	for dis in $DIS
	do 
	    ./work.sh $fname $dim $dis
	done
    done
done
