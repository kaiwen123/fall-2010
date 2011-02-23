# Usage: 
#      python filtering.py [transformed-data-file] [original-query-file]
# Filtering using the rule : $y' * (A^{-1})' w v' A^{-1} * y$
# Here we need to access the original query and transformed data; 
# Files accessed in this program:
# param.txt - the parameter file;
# query.txt - the original query file; 
# data.t - transformed data file; 
# 
# NOTE: For efficiency of processing, we can extract the queried result data
# to a file so that we only need to process this data file; 

import sys, os, time
import numpy as np
import c_ext                    # c matrix implementation implementation; 
from cvxmod import *

def main(): 
    fparam = open("param.txt", "r") # parameter file;

    total_sec = 0.0             # Total query time; 
    total_result = 0            # Total number of filtering results;
    #### Read in parameters; 
    k = int(fparam.readline().split(" ")[0])  # Dimension of matrix; 
    ko = k - 2                                # Original data dim; 
    A = [] 
    for i in range(k):
        #print fparam.readline().strip().split()
        A.append([float(j) for j in fparam.readline().strip().split()])
    A = matrix(A).T
    invA = matrix(np.linalg.inv(A))
    v = eye(k)[:,-1]
    total_access = 0

    # Read in the whole file as a matrix; 
    d = np.fromfile(sys.argv[1], dtype=float, sep=" ")
    cold = k * 2 + 3
    rowd = len(d) / cold
    d = np.reshape(d, (rowd, cold), order="C")

    q = np.fromfile(sys.argv[2], dtype=float, sep=" ")
    colq = ko * 2 + 3 
    rowq = len(q) / colq
    q = np.reshape(q, (rowq, colq), order="C")
    # Get original query
    for lineq in range(rowq):
        Qs = []
        Qe = []

        for i in range(len(q[lineq,:])):
            if i > 2 and i % 2 == 1: # lower boundary
                Qs.append(q[lineq, i])
            elif i > 2 and i % 2 == 0: # higher boundary
                Qe.append(q[lineq, i])

        Wl = -1 * eye(k)
        Wh = eye(k)

        for i in range(ko):
            Wl[ko, i] = Qs[i]
            Wh[ko, i] = -1 * Qe[i]
        W = concathoriz(Wl[:,0:ko], Wh[:,0:ko])

        for lined in range(rowd):
            total_access += 1
            ItemID = int(d[lined, 1])
            Dl = []
            Dh = []
            #print "d = ", d[lined, 3:]
            #Put lower and higher boundary into different arrays; 
            for i in range(len(d[lined,:])):
                if i > 2 and i % 2 == 1: # lower boundary
                    Dl.append(d[lined, i])
                elif i > 2 and i % 2 == 0: # higher boundary
                    Dh.append(d[lined, i])
                    
            yl = matrix(Dl)
            yh = matrix(Dh)
            #print yl, yh
            res1 = (yl.T * invA.T * W) * (v.T * invA * yl)[0]
            res2 = (yh.T * invA.T * W) * (v.T * invA * yh)[0]
            #print v.T * invA
            #print yl
            
            #print yl.T * invA.T * W
            #print yh.T * invA.T * W
            #print v.T * invA * yl
            #print v.T * invA * yh
            #print res1, res2
            y = d[lined,3:]
            #print y
            yc = c_ext.floatArray(2*k)
            invAc = c_ext.floatArray(k*k)
            Wc = c_ext.floatArray(k*2*ko)
            for x in range(2*k):
                 yc[x] = y[x]
                
            for x in range(k):
                for x1 in range(k):
                    invAc[k*x + x1] = invA[k*x + x1] 
                for x2 in range(2*ko):
                    Wc[2*ko*x + x2] = W[2*ko*x2 + x]
                    #print Wc[2*ko*x + x2]

            t_start = time.clock() # Start timing;
            res = c_ext.validate0(yc, invAc, Wc, k);
            if res==1: 
                total_result += 1
                #print ItemID
                
            total_sec += (time.clock() - t_start) # Summing up time;   
            # # Do the verification
            # for i in range(cols(W)):
            #     w = W[:, i]
            #     yl = matrix(Dl)
            #     yh = matrix(Dh)
            #     T = invA.T * w * v.T * invA

                
            #     # ----------------------------------------
            #     # Now put data into c array; 
            #     # Define arrays 
            #     ylc = c_ext.floatArray(k)
            #     yhc = c_ext.floatArray(k)
            #     Tc = c_ext.floatArray(k*k)
            #     for x in range(k):
            #         ylc[x] = Dl[x]
            #         yhc[x] = Dh[x]
            #         for m in range(k):
            #             Tc[x*k+m] = T[x*k+m]

            #     t_start = time.clock() # Start timing; 

            #     result_l = c_ext.validate(ylc, Tc, k)
            #     result_h = c_ext.validate(yhc, Tc, k)
            #     if (not (result_l<0 and result_h<0)):
            #         passed = False
            #         break

            #    total_sec += (time.clock() - t_start) # Summing up time; 

            
                #print ItemID
    print " -filter-", total_sec, total_result #, total_access


if __name__ == "__main__":
    main()
