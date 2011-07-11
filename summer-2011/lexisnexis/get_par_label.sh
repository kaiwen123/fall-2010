#!/bin/bash
# this script can be used to get paragraph labels in the original document. 
# so that we can be sure of the number of actual paragraphs in the document. 

# This will get paragraph label from xml input file. 
egrep -o '[a-z]{0,4}para_[0-9]+' $1

# This one contains all paras excluding the headnote and the footnotes. 
egrep -o '\"para_[0-9]+' $1 
