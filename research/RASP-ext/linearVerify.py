# Linear verification of query;
# Usage : 
#       python linearVerify.py [data-file] [query-file]
import sys, os
import numpy as np
from cvxmod import *

def main():
    fdata = open(sys.argv[1], "r")
    fquery = open(sys.argv[2], "r")
    
    for linequery in fquery:
        
        dquery = linequery.strip().split()[3:]

        fdata.seek(0)
        for linedata in fdata:
            ItemID = linedata.strip().split(" ")[1]
            ddata = linedata.strip().split(" ")[3:]
            passed = True

            x = len(dquery)
            for i in range(0, x, 2):
                if (float(ddata[i]) < float(dquery[i])) or \
                        float(ddata[i+1]) > float(dquery[i+1]):
                    passed = False
                    break
                
            if passed == True:
                print ItemID

    # Close the files; 
    fdata.close()
    fquery.close()

if __name__ == "__main__":
    main()
