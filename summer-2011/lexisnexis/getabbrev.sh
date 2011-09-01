# This script is used to extract abbrevations from input files. 
# Four steps are involved in gettting the abbrevations, first get the 
# abbrevation using the script, then count the occurances of each abb; 
# finally, sort and count account by number of occurances from largest 
# to smallest. 
perl abbrev.pl < /data/millions/batch1/out1 | sort -i | uniq -c | sort -nr | less
