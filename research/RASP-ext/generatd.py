# generatd.py -- A random query generator
# Usage: 
#      ./python generatd.py [dim] [num-data]
# 
import numpy as np
import os, sys

"""
Random data generator function defination
k - dimension of query; 
n - number of queries;
r - range of query;
output - random query matrix 

"""
def main():
    k = int(sys.argv[1])        # Dimension
    n = int(sys.argv[2])        # Number of data; 

    Id = 0
    np.random.seed()                   # Use system time as random seed

    for i in range(0, n):
        mdata = "1 " + str(Id) + " " + str(k)
        Id += 1
        for j in range(0, k):
             d = np.random.uniform(low=-2, high=2)
             mdata += " " + str(d) + " " + str(d)
             
        print mdata 


if __name__ == "__main__":
    main()
