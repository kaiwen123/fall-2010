# This file is used to filter out the data item with the preliminary
# query result. 

# Usage: 
#      python silt.py [prelimiary-query-result] [transformed-data]

# This program will print out the data items of the prelimiary query.
import os, sys

def main():
    fqresult = open(sys.argv[1], "r") # sorted query result file;
    ftdata = open(sys.argv[2], "r") # transformed data file;    
    linenum = 0                 # line number in trans data file

    for lineresult in fqresult:
        lineres = int(lineresult.strip()) # result item number
        #print lineres
        while (lineres > linenum):
            ftdata.readline()
            linenum += 1
        print ftdata.readline(),
        linenum += 1
   
    fqresult.close()
    ftdata.close()
if __name__ == "__main__":
    main()
