from socket import *
from time import time, sleep
import sys
BUFSIZE = 4096

class ConnectionHandler:
    ''' Handles connections from data owner, authorized data user, 
    and the communication between the data server on the cloud. '''
    def __init__(self, host, port):
        ''' Initialize connection parameters. ''' 
        self.__HOST = host
        self.__PORT = port
        self.__ADDR = (self.__HOST,self.__PORT)
 
    def makeConnection(self):
        ''' Make SOCKET connection. ''' 
        sock = socket(AF_INET,SOCK_STREAM)
        sock.connect(self.__ADDR)
        return sock
    
    def closeConnection(self, sock):
        ''' Close connection to the server. '''
        try: 
            sock.sendCmd('BYE', sock)        
            return True
        except:
            print 'No connection to the server.'
            return False
 
    def sendCmd(self, cmd, sock):
        ''' Send operations to the server. 
        Commands include:
        1, connection test: 'TEST'. 
        2, data upload: 'UPLOAD'. 
        3, data query: 'QUERY'.
        4, get data stores from proxy server. 'STORES'
        ''' 
        sock.send(cmd)
 
    def getResults(self, sock):
        ''' Get results returned from data server. ''' 
        data = sock.recv(BUFSIZE)
        return data

    def sendData(self, data, sock):
        ''' Send data to server. ''' 
        data_lines = data.strip().split('\n')
        for line in data_lines:
            sock.send(data)

    def testConnection(self, sock):
        '''Test the connection to the data server. '''
        self.sendCmd('TEST', sock) 
        retries = 5 
        while True:
            status = self.getResults(sock) 
            type(status)
            if status.find('OK!') != -1: 
                return True
            else: 
                print 'Connection to server failed, retrying......'
                retries -= 1; 
                if retries <= 0: 
                    return False
