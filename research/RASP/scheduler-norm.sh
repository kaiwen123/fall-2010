#!/bin/sh
# This is the experiment scheduler. 
# With this file, we can automatically deal with a group of
# experiments. 

for file in norm.*
do
    fname=$file
    fregex='norm.k*.d*.3c'

    if [ -f $fname ]; then
	dsize=`echo $file | cut -d"." -f2 | grep -r -o "[0-9]*"`
	dim=`echo $file | cut -d"." -f3 | grep -r -o "[0-9]*"`
	dis="0.5"
	size=$(($dsize * 1000))
    
	echo "Working on file : " $fname
	./work.sh $fname $dim $dis
    fi
done