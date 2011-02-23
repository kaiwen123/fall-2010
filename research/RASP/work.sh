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
FPARAM=param.txt		# Parameter file;
FQUERY=query.txt		# Original query file;
FDATA=$1			# Original data file; 
FDNORM=$FDATA.n			# Normalized data 
FCOUNT=records.txt		# Result records;
TQMETHOD=1		        # Query transform method, naive(0)/optimization(0)
VNORMLIZE=0			# If the data need normalization; yes(1)/no(0)

CAPACITY=20
QTYPE=intersection
VDIM=$2				# Dimension
VNUM=100			# Number of queries; 
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
if [ ! -x $RTREEPROG ]
then
    echo "Compiling ....."
    make
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
    python transd.py $FQUERY
else
    echo "Transforming Query with optimization method......"    
    python transq.py $FQUERY
fi

#%%%%%%%%%%%%%%%%%%%% Query original data; 
echo "Querying original data ......"
$RTREEPROG $FDNORM $FQUERY $CAPACITY 

#%%%%%%%%%%%%%%%%%%%% Query transformed data; 
echo "Querying transformed data ......"
$RTREEPROG $FDATA.t $FQUERY.t $CAPACITY $FQUERY 

#%%%%%%%%%%%%%%%%%%%% Cleaning temporary files;
echo "Cleaning temperary files %%%%%%%%%%"
#$RM -rf *.dat *.idx *.tmp query.* *.t *.n
#$RM -rf $FNORM
echo "End of processing ... ... "
echo "-------------------------" >> $FCOUNT
