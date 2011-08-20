#!/bin/bash
# @brief This script randomly generates random test input xml files.
# @param $1 source input files. 
# @param $2 how many docs to extract.
INPUT=$1;
NUM=$2;
for i in $(seq $NUM); do
    DOC=$RANDOM;
    echo 'Extracting doc' $DOC input_$i.xml;
    sed ''"$DOC"'q;d' $INPUT > input_$i.xml;
#    sed ''"$RANDOM"'q;d' $INPUT | tee -a random_sentence_test.xml;
done; 