# generatq.py -- A random query generator
# Usage: 
#      ./python generatq.py [dim] [num-query] [distance]
# 
import random as rd 
import numpy as np
import os, sys

"""
Query generator function defination
k - dimension of query; 
n - number of queries;
r - range of query;
output - random query matrix 

"""
def main():
    k = int(sys.argv[1])        # Dimension
    n = int(sys.argv[2])        # Number of queries; 
    r = float(sys.argv[3])      # Distance 

    rd.seed()                   # Use system time as random seed
    i = 0;
    for i in range(0, n):
        mquery = "2 " + str(i) + " " + str(k)
        for j in range(0, k):
             q = rd.uniform(-1, 1)
             mquery += " " + str(q) + " " + str(q + r)
        print mquery 

if __name__ == "__main__":
    main()
