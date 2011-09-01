#!/bin/bash
# This script is used to run random sentence cutting test. 
# steps include:
# 1, randomly get a xml input from the input source; 
# 2, redirect the input xml file into the sentence cutting script. 
# 3, manually check the sentence cutting result. 
# If there is any errors, scripts should be updated to correct the
# error and then do the test again. 

# check parameter errors. 
if [ $# -ne 2 ]; then 
    echo "Usage: $0 <num_random_test> <input_xml_repository>" ;
    exit 1 ; 
fi ; 

NUM=$1 ;
XML_SRC=$2 ; 

# do the random test. 
for i in $(seq $NUM); do
    sed ''"$RANDOM"'q;d' $XML_SRC | tee -a random_sentence_test.xml | perl xml2text.pl | perl para2sentences.pl | less 
done; 
