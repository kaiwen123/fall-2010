"""
transq - a query transformer; 
Usage:
     python transq.py striped-query-file
This function transforms the original query to transformed format,
according to the data transformation matrix A. 
input: 
\tA - transformation matrix; 
\tQs - start vector of the original query for each query; 
\tr - a random variable within (0,1]
"""
from cvxmod import *
from cvxopt import solvers
import numpy as np
import sys, os

def main():
    f = open("param.txt")

    k = int(f.readline().split(" ")[0])       # Dimension of matrix; 
    A = [] 
    for i in range(k):
        #print f.readline().strip().split()
        A.append([float(j) for j in f.readline().strip().split()])
    A = matrix(A).T
    v = float(f.readline())

    # Transforming query line by line;
    f.close()
    
    f = open(sys.argv[1]) # argv[1] is the name of the query file; 
    Q = []
    for line in f:
        Q.append([float(j) for j in line.strip().split()])
        Q = matrix(Q).T
        transformquery(A, Q)
        Q = []
    f.close()

def transformquery(A, Q):   
    k1, k1 = size(matrix(A))
    k = k1 - 2
    Qs = []
    Qe = []
    itemID = int(Q[1])
    for i in range(len(Q)):
        if (i > 2) and (i % 2==1):
            Qs.append(Q[i])
        elif (i > 2) and (i%2 ==0):
            Qe.append(Q[i])

    detA = np.linalg.det(A)

    Ainv = param('Ainv', k1, k1, value = matrix(np.linalg.inv(A)))

    W1 = -1 * eye(k1)
    W2 = 1 * eye(k1)
    for i in range(k):
        W1[k,i] = Qs[i]
        W2[k,i] = -1 * Qe[i]
    W = concathoriz(W1[:,0:k], W2[:,0:k])

    # Writing the generated query to query file;     
    mquery = "2 " + str(itemID) + " " + str(k1) # multidimensional query string;
    # convex problem with mnimizing. 
    for x in range(0, k1):
        y = optvar('y', k1, 1)
        c = param('c', k1, 1, value = eye(k1)[:,x])
        
        p = problem()        
        p.constr = []
        # Adding constrains; 
        for i in range(0, 2*k):
            p.constr.append(tp(param('w' + str(i), \
                                     k1, 1, value = W[:,i])) * \
                                     Ainv * y <= 0)
            # print "w" + str(i) + ":", 
            # print param('w' + str(i), k1, 1, value = W[:,i]).value
            
        # Additional Constrains; 
        # The k+1 element equals 1
        # The k+2 element is in (0,1]
        w1w = param('w1w', 1, k1, value = Ainv[-2,:])
        w2w = param('w2w', 1, k1, value = Ainv[-1,:])
        # print "w1w = ", w1w.value 
        # print "w2w = ", w2w.value

        p.constr.append(w1w * y == 1)
        p.constr.append(-1 * w2w * y < 0)
        p.constr.append(w2w * y <= 1)
        
        solvers.options['show_progress']=True 
        # solvers.options['feastol']= 1e-6
        
        # Error handling process; .. .. ..
        res1 = 0
        res2 = 0 
        numtry = 1              # Number of tries; 
        flog = open("error.log.txt", "a") # Log file
        # In order to get the optimuim value, we control the parameter feastol 
        # so that a final value will be got anyway; 
        while True: 
            try:
                solvers.options['feastol']=1e-5 * numtry 
                p.objective = minimize(tp(c) * y)
                p.solve(True)
                res1 = value(tp(c) * y)
                #flog.write("get the minimuim value: " + str(res1) + "\n")
                break
            except(OptvarValueError):
                #flog.rite("Error happens: " + " Min XXXXXXXXXXXXXXXX\n")
                #flog.write("numtry = " + str(numtry) + "\n")
                if numtry > 1e5: 
                    #flog.write("Can't resolve the problem... \n")
                    res1 = -1000
                    break 
                numtry *= 1.2
                #flog.write("Retrying ... ... ...\n")

        numtry = 1
        mquery += " " + str(res1)

        while True:
            try:
                solvers.options['feastol']=1e-5 * numtry            
                p.objective = maximize(tp(c) * y)
                p.solve(True)
                res2 = value(tp(c) * y)
                #flog.write("Get the maximuim value : " + str(res2) + "\n")
                break
            except(OptvarValueError):
                #flog.write("Error happens: " + " Max !!!!!!!!!!!!!!!!\n")
                #flog.write("numtry = " + str(numtry) + "\n")
                if numtry > 1e5: 
                    #flog.write("Can't resolve the problem... \n")
                    res2 = 1000
                    break 
                numtry *= 1.2
                #flog.write("Retrying ... ... ... \n")
        flog.close()
        numtry = 1
        mquery += " " + str(res2)
        
    print mquery   

if __name__ == "__main__":
    main()
