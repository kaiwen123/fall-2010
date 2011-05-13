#!/usr/bin/python 
# @brief This script is used to tranform facebook binary data to
# multiple valued data. 
# Rules:
#+----------+---------+----------+-----------+---------+
#| Account  |   YES   | YES      |   NO      |  NO     |
#|----------+---------+----------+-----------+---------+
#| Nobody   |   YES   | NO       |   YES     |  NO     |
#+----------+---------+----------+-----------+---------+
#| New Value| Everyone| f/ff     |   ERROR   | Only ME |
#+----------+---------+----------+-----------+---------+
# Assign value according to the following: 
# private: 0
# friend only: 1
# friend of friend: 2
# everyone: 3

# read in the data into hash table with key as the user id and value
# as the binary item settings. 
fuser = open(sys.argv[1])
fnobody = open(sys.argv[2])

user = {}
nobody = {}
for user in fuser:
    u = user.strip().split(' ')
    uid = u[0]
    user[uid] = u[1:]

for nobody in fnobody:
    u = nobody.strip().split(' ')
    uid = u[0]
    nobody[uid] = u[1:]

# Build the multiple value data for the generated data. 
for key in user.keys():
    print key
    if nobody.has_key(key):
        binuser = user[key]
        binnobody = nobody[key]
        if len(binuser) == len(binnobody):
            length = len(binuser)
            for i in range(length):
                print binuser[i], binnobody[i]
                bu = binuser[i]
                bn = binnobody[i]
                if bu and bn: print 2
                if bu and no bn: print 1
                if not bu and not bn: print 0
        else:
            print 'binary data is of wrong format.'
            continue
