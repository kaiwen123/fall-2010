#!/bin/sh

# ------------------------------------------------------------
# This is the wrapper script to execute the whole experiment 
# Version: 3.0.0
# License: GPL
# Author:  Shumin Guo
# Contact: gsmsteve@gmail.com
# ------------------------------------------------------------

RM=/bin/rm
RTREEPROG=./RTreeQuery		# RTree program;
EXHAUSTPROG=./Exhaustive	# Exhaustive linear scan program;

FPARAM=param.txt		# Parameter file;
FQUERY=query.txt		# Original query file;
FDATA=$1			# Original data file; 
FDNORM=$FDATA.n			# Normalized data 
FCOUNT=records.txt		# Result records;
TQMETHOD=0		        # Query transform method, naive(0)/optimization(0)
VNORMLIZE=0			# If the data need normalization; yes(1)/no(0)

CAPACITY=100
QTYPE=intersection
VDIM=$2				# Dimension
VNUM=1000			# Number of queries; 
VDIS=$3				# Distance of dimension; 


if [ $# -ne 3 ]; then 
    echo "Usage: ./doit.sh [data-file] [dimension] [distance]"
    exit 1
fi 

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Parameter print out. 
echo "==============================================" 
echo "Experiment Parameters:"
echo "Data file:           " $FDATA
echo "RTree node capacity: " $CAPACITY
echo "Dimension:           " $VDIM
echo "Numver of queries:   " $VNUM
echo "Distance of queries: " $VDIS
echo "Starting now ********************"
echo 

# Recompile the program.
if [ ! -x $RTREEPROG -o ! -x $EXHAUSTPROG ]
then
    echo "compiling ....."
    ./compile.sh
fi

echo "Generating parameters......"
python generatp.py $VDIM > $FPARAM

echo "generating query......"
python generatq.py $VDIM $VNUM $VDIS > $FQUERY

if [ $VNORMLIZE -eq 1 ]
then 
    echo "Normalizing the original data... ... "
    python normdata.py $FDATA $VDIM > $FDNORM
else
    cp $FDATA $FDNORM 
fi

# Transform data; 
echo "Transforming data......"
python transd.py $FDNORM > $FDATA.t

# Transform query; 
echo "Transforming queries......"
if [ $TQMETHOD -eq 0 ]
then 
    echo "Transforming Query with NAIVE method......" 
    python transd.py $FQUERY > $FQUERY.t    
else
    echo "Transforming Query with optimization method......"    
    python transq.py $FQUERY > $FQUERY.t
fi

#%%%%%%%%%%%%%%%%%%%% Query original data; 
echo "Querying original data ......"
$RTREEPROG $FDNORM $FQUERY $CAPACITY 2>>$FCOUNT

#%%%%%%%%%%%%%%%%%%%% Query transformed data; 
echo "Querying transformed data ......"
$RTREEPROG $FDATA.t $FQUERY.t $CAPACITY $FQUERY 2>>$FCOUNT

#%%%%%%%%%%%%%%%%%%%% Cleaning temporary files;
echo "Cleaning temperary files %%%%%%%%%%"
$RM -rf *.dat *.idx *.tmp query.* *.t
$RM -rf $FNORM
echo "End of processing ... ... "
echo "-------------------------" >> $FCOUNT
