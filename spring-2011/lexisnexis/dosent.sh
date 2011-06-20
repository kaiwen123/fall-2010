#!/bin/bash
# @brief This script is used to run sentence cutting over a group of input data. 
# @input Number of input files. 
# @output Sentences from the input files. 
NUM=$1;
for i in $(seq $NUM); 
do
    echo "processing doc" input_$i.xml;
    perl conv_to_text.latest input_$i.xml _1 | perl sentence_cutting/tosentence.pl > split_$i.sen; 
done
