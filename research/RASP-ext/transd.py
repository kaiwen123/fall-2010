# -*- mode: python; fill-column: 70; comment-column: 50; -*-
# Usage: 
#      python transd.py [norm-data] [dim]
# This file is used to transform the original data into the
# transformed format, according to the following formula:
#             
#                 (X)
#           Y = A |1| 
#                 (v)  
# 
# Of the above transformation formula: 
# Y is the transformed data vector;
# X is the original data vector; 
# A is the transformation matrix, which is inversable; 
# v is a random variable with (0,1];
 
"""
Data transformer 
"""

import os, sys
import numpy as np
from cvxmod import *

def main():
    f = open("param.txt")

    k = int(f.readline().split(" ")[0])       # Dimension of matrix; 
    A = [] 
    for i in range(k):
        #print f.readline().strip().split()
        A.append([float(j) for j in f.readline().strip().split()])
    A = matrix(A).T
    v = float(f.readline())

    f.close()

    fin = open(sys.argv[1], 'r')
    flog = open("error.log.txt", "a")
    # Here comes the transformation; 
    m = []
    for line in fin: 
        m.append([float(i) for i in line.strip().split(" ")[3:]])

        x0 = []                                   # The original x axis;
        y0 = []                                   # The original y axis; 
        for i in range(len(m[0])):
            if i % 2 == 0:
                x0.append(m[0][i])
            else:
                y0.append(m[0][i])
                
        # Add an additional dimension and noise perturbation variable; 
        x0.append(1)
        x0.append(v)
        y0.append(1)
        y0.append(v)
        
        try:
            x1 = np.dot(A, x0)                  # The transformed x axis; 
            y1 = np.dot(A, y0)                  # The transformed y axis; 
        except(ValueError):
            flog.write("transd.py: Error happened while transforming axis \n")
        
        # Now, let's combine the transformed axis to an array;
        # And then we can output the line into the transformed file;
        line = line.strip().split()
        
        print line[0], line[1], int(line[2]) + 2,
        for i in range(len(x1)):
            print x1[i], y1[i],
        print
        m = []

    fin.close()
    flog.close()
if __name__ == "__main__":
    main()
