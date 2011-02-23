"""
Usage: 
     python normdata.py original-data [dim]
This is used to normalize the input data
In order to calculate the mean and standard 
deviation value of the data values, we combine each 
two column which is one dimension as far as the data 
is concerned.

"""
import numpy, os, sys

def main():
    dim = int(sys.argv[2])
    colnum = dim * 2
    samplesize = 20000
    counter = 1 

    f = open(sys.argv[1])
    # Calculate the mean and standard deviation of each column
    m = []
    for line in f:
        if counter > samplesize: 
            break 
        m.append([float(i) for i in line.strip().split()[3:]])
        counter += 1
    f.close()
    
    m = numpy.matrix(m) 

    # get the mean and standard deviation value;
    mean = m.mean(axis = 0).tolist()[0]
    std = m.std(axis = 0).tolist()[0]
    f = open(sys.argv[1])

    for line in f:
        for i in line.strip().split()[0:3]:
            print i, 

        x = []
        x.append([float(i) for i in line.strip().split()[3:]])
        x = x[0]
        for i in range(colnum):
            if i % 2 == 0: 
                meanval = (mean[i] + mean[i + 1]) / 2
                stdval = numpy.sqrt(std[i]**2 + std[i + 1]**2)
            #print meanval, " ", stdval

            print (float(x[i]) - meanval) / stdval, 
            #print x[i],
        print 
    f.close()
if __name__ == "__main__":
    main()
