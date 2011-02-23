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
    ftdata = open(sys.argv[1], "r") # transformed data file; 
    fquery = open(sys.argv[2], "r") # original query file; 
    
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
    # Get original query
    for linequery in fquery:
        Q = []
        Qs = []
        Qe = []

        Q.append([float(j) for j in linequery.strip().split()])
        Q = Q[0]                # pickup the first line; 

        for i in range(len(Q)):
            if i > 2 and i % 2 == 1: # lower boundary
                Qs.append(Q[i])
            elif i > 2 and i % 2 == 0: # higher boundary
                Qe.append(Q[i])
    
        Wl = -1 * eye(k)
        Wh = eye(k)

        for i in range(ko):
            Wl[ko, i] = Qs[i]
            Wh[ko, i] = -1 * Qe[i]
        W = concathoriz(Wl[:,0:ko], Wh[:,0:ko])

        # Let's filter each line of data to each query in the query
        # file; 
        ftdata.seek(0)
        for linedata in ftdata:
            total_access += 1
            D = []              # Data array
            Dl = []             # Lower dimension
            Dh = []             # Higher dimension
            D.append([float(j) for j in linedata.strip().split()])
            D = D[0]
            ItemID = int(D[1])
            passed = True         # pass

            # Put lower and higher boundary into different arrays; 
            for i in range(len(D)):
                if i > 2 and i % 2 == 1: # lower boundary
                    Dl.append(D[i])
                elif i > 2 and i % 2 == 0: # higher boundary
                    Dh.append(D[i])
            # Do the verification
            for i in range(cols(W)):
                w = W[:, i]

                # Lower boundary verification 

                # ----------------------------------------
                # Now put data into c array; 
                # Define arrays 
                # ylc = c_ext.floatArray(k)
                # yhc = c_ext.floatArray(k)
                # wc = c_ext.floatArray(k)
                # invAc = c_ext.floatArray(k*k)
                # vc = c_ext.floatArray(k)
                # for x in range(k):
                #     ylc[x] = Dl[x]
                #     yhc[x] = Dh[x]
                #     wc[x] = w[x]
                #     vc[x] = v[x]
                #     for m in range(k):
                #         invAc[x*k+m] = invA[x*k+m]

                # t_start = time.clock() # Start timing; 

                # result_l = c_ext.validate(ylc, invAc, wc, vc, k)
                # result_h = c_ext.validate(yhc, invAc, wc, vc, k)
                # if (not (result_l<0 and result_h<0)):
                #     passed = False
                #     break


                # ----------------------------------------
                yl = matrix(Dl)  
                yh = matrix(Dh)  

                t_start = time.clock() # Start timing; 

                result = (yl.T * (invA).T * w * v.T * invA * yl) [0]
                #print result_l, result
                #print (invA).T * w
                if result > 0: 
                    passed = False
                    break
                # Higher boundary verification
                result = (yh.T * (invA).T * w * v.T * invA * yh) [0]
                #print result_h, result
                if result > 0:
                    passed = False
                    break
                total_sec += (time.clock() - t_start) # Summing up time; 

            if passed:
                total_result += 1
                #print ItemID
    print " -filter-", total_sec, total_result, total_access
    #print "total time of filtering : ", total_sec                
    fparam.close()
    fquery.close()
    ftdata.close()

if __name__ == "__main__":
    main()
