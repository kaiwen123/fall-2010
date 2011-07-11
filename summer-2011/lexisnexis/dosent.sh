#!/bin/bash
# @brief This script is used to run sentence cutting over a group of input data. 
# @input Data set to retrieve data from.
# @input Number of input files. 
# @output Sentences from the input files. 

INPUT=$1;
NUM=$2;

for i in $(seq $NUM); do
    DOC=$RANDOM;

    echo 'Extracting doc' $DOC input_$i.xml;
    sed ''"$DOC"'q;d' $INPUT > input_$i.xml;

    echo "processing doc" input_$i.xml;
    perl xml2text.pl input_$i.xml _$i | perl sentence_cutting/tosentence.pl > split_$i.sen; 
done
