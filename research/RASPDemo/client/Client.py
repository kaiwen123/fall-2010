#!/usr/bin/env python
# a simple TCP client
 
from socket import *
from time import time
from time import sleep
import sys
BUFSIZE = 4096
 
class CmdLine:
    def __init__(s,host):
        s.__HOST = host
        s.__PORT = 10001
        s.__ADDR = (s.__HOST,s.__PORT)
        s.__sock = None
 
    def makeConnection(s):
        s.__sock = socket( AF_INET,SOCK_STREAM)
        s.__sock.connect(s.__ADDR)
 
    def sendCmd(s, cmd):
        s.__sock.send(cmd)
 
    def getResults(s):
        data = s.__sock.recv(BUFSIZE)
        print data
  
if __name__ == '__main__':
    conn = CmdLine('localhost')
    conn.makeConnection()
    d = open("data")
    for line in d:
        #print "local: ", line 
        conn.sendCmd(line)

    conn.getResults()
    conn.sendCmd('BYE')
