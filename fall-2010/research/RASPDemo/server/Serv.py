#!/usr/bin/env python
# Multithreaded Socket Server

# Problems.
# Unhandled exception in thread started by <function handler at 0xe83398>
# Traceback (most recent call last):
#   File "Serv.py", line 11, in handler
#     data = clientsock.recv(BUFSIZ)
# socket.error: [Errno 104] Connection reset by peer

from socket import *
from threading import *
import thread
import numpy as np
import rasp_demo as demo

def handler(clientsock,addr):
    BUFSIZ = 4096
    while 1:
        data = clientsock.recv(BUFSIZ)
        print data
        if data == "BYE": 
            break 
        result = processdata(data)
        clientsock.send(result)
        
    clientsock.close()

def processdata(data):
    # First let's decide if this is insert or query.
    operation = data.strip().split(" ")[0]
    print operation
    if operation == '1':
        result = insertData(data)
    if operation == '2':
        result = queryData(data)
    return result

def insertData(data):
    #print "Inserting data..." 
    result = demo.insertData(data)
    return result # "INSERT SUCCESSFUL"

def queryData(data):
    #print "Querying data..."
    return "Query result is:"

if __name__=="__main__": 
    HOST = 'localhost'
    PORT = 21567
    BUFSIZ = 1024
    ADDR = (HOST, PORT)
    serversock = socket(AF_INET, SOCK_STREAM)
    serversock.bind(ADDR)
    serversock.listen(10)
    
    while 1:
        print "waiting for connection"
        clientsock, addr = serversock.accept()
        print "connected from:", addr
        thread.start_new_thread(handler, (clientsock, addr))
