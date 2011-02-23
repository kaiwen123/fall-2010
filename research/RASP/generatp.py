# This is the parameter generation file; 
# This file contains code of generating the following parameters: 
# A - A random transformation matrix and 
# v - A random variable with value within (0,1]; 

import numpy as np
import sys, os

"""
Parameter generator
Usage: 
    python generatp.py [dimension]

n is the dimension of the matrix A; 
returns: 
\tA - the transformation matrix; 
\tv - A random variable for data perturbation; 

In the end, dump the parameters to the standard output; 
The format is : 

k k
A00 A01 A02 ... A0k
A10 A11 A12 ... A1k
... ... ... ... ...
... ... ... ... ...
Ak0 Ak1 Ak2 ... Akk
v

k - dimension 
Axx - value 
v - random variable 
"""
def main():
    k = int(sys.argv[1])
    k = k + 2
    finva = open("inva.txt", "w")
    A =np.random.uniform(low=0, high=10, size=(k, k))
    invA = np.asarray(np.linalg.inv(np.mat(A)))
    #print np.mat(A) * np.matrix(invA)
    v = np.random.rand()
    while not(v > 0.02 and v <= 1): 
        v = np.random.rand()

    print k, k
    finva.write(str(k)+" "+str(k)+"\n")
    for i in range(0, k):
        for j in range(0, k): 
            print A[i][j],
            #print invA[i][j]
            finva.write(str(invA[i][j])+" ")
        print 
        finva.write("\n")
    print v
    finva.close()

if __name__ == "__main__":
    main()
