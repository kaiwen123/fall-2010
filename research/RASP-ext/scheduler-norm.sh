#!/bin/sh
# This is the experiment scheduler. 
# With this file, we can automatically deal with a group of
# experiments. 

DIMS="5 6 7 8 9 10"		# Dimensions of data;
DSIZES="20"		# Data item numbers; 
DIS="0.5"

for dim in $DIMS
do
    for dsize in $DSIZES
    do
	fname=norm.k${dsize}.d${dim}.3c
	size=$(($dsize * 1000))

	# Generate random data; 
	echo 
	echo "*******************************************"

	#python generatd.py $dim $size > $fname
	
	for dis in $DIS
	do 
	    ./work.sh $fname $dim $dis
	done
    done
done
