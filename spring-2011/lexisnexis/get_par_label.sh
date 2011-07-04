#!/bin/bash
# this script can be used to get paragraph labels in the original document. 
# so that we can be sure of the number of actual paragraphs in the document. 

egrep -o '[a-z]{0,2}para_[0-9]+' input_1.xml
