import numpy as np
import random as rd
import sys, os
import ConnectionHandler as Handler

class DataUser:
    ''' This class represents the data owner of the demo system. 
    Functionalities include: 
    1, Generation of parameters. 
    2, Generation of original data to be outsourced. '''
    def __init__(self):
        ''' Initilize the data generator. Global parameters should
        be initilized. '''
        # Proxy server client. 
        self.proxy_server_connector = \
            Handler.ConnectionHandler('localhost', 22222)
        self._socket = self.proxy_server_connector.makeConnection()
        self.current_user = ''

    def origdata_gen(self, k, n): 
        ''' Generates original data. 
        k - dimension of query; 
        n - number of queries;
        r - range of query;
        output - random query matrix '''
        Id = 0
        np.random.seed()        # Use system time as random seed
        mdata = ''
        for i in range(0, n):
            mdata += '1 ' + str(Id) + ' ' + str(k)
            Id += 1
            for j in range(0, k):
                d = np.random.uniform(low=-2, high=2)
                mdata += ' ' + str(d) + ' ' + str(d)
            mdata += '\n'
        return mdata

    def origquery_gen(self, k, n, r):
        ''' Original Query generator. 
        k - dimension of query; 
        n - number of queries;
        r - range of query;
        output - random query matrix '''
        rd.seed()               # Use system time as random seed
        mquery = ''
        for i in range(0, n):
            mquery += "2 " + str(i) + " " + str(k)
            for j in range(0, k):
                q = rd.uniform(-1, 1)
                mquery += " " + str(q) + " " + str(q + r)
            mquery += '\n'
        return mquery

    def testProxyServerConnection(self):
        ''' Test if the proxy server is still alive. ''' 
        return self.proxy_server_connector.testConnection()

    def connecttoProxyServer(self):
        ''' Make connection to the proxy server. ''' 
        self._socket = self.proxy_server_connector.makeConnection()

    def closeConnectiontoProxyServer(self):
        ''' Make connection to the proxy server. ''' 
        self.proxy_server_connector.closeConnection(self._socket)

    def sentDatatoProxyServer(self, data):
        ''' Send the original data to the proxy server. ''' 
        self.proxy_server_connector.sendData(data,self._socket)

    def sentQuerytoProxyServer(self, query):
        ''' Send the original query to the proxy server. ''' 
        self.proxy_server_connector.sendData(query, self._socket)

    def userRegistration(self, username, passwd):
        ''' Users need to register before uploading and querying
        data to the data server. And the data server will use the 
        user name as one parameter to manage the transformed data 
        on the data server. ''' 
        print 'Data user registration request:', username
        reg_str = 'REG ' + username + ' ' + passwd
        self.proxy_server_connector.sendCmd(reg_str, self._socket)
        return self.proxy_server_connector.getResults(self._socket)

    def userLogin(self, username, passwd):
        ''' Login to the proxy server. ''' 
        self.current_user = username
        login_str = 'LOGIN ' + username + ' ' + passwd
        self.proxy_server_connector.sendCmd(login_str, self._socket)
        return self.proxy_server_connector.getResults(self._socket)

    def getDataStores(self):
        ''' Return all the data store for the current user. ''' 
        req_str = 'STORES ' + self.current_user
        self.connecttoProxyServer()
        self.proxy_server_connector.sendCmd(req_str, self._socket)
        return self.proxy_server_connector.getResults(self._socket)

if __name__ == '__main__':
    data_owner = DataUser() 

    # if there is no connection to the server, make it. 
    if not data_owner.testProxyServerConnection(): 
        data_owner.connecttoProxyServer()
        print 'Successfully connected to the proxy server! '

    # user registration. 
    reg_resp = data_owner.userRegistration('simon', '123456')
    # user login to the proxy server. 
    login_resp = data_owner.userLogin('simon', '123456')
    print login_resp

    # original data generation. 
    k = 5
    n = 10
    data = data_owner.origdata_gen(k, n)
    datalines = data.split('\n')
    data = ''
    print 'Data upload test ...', 
    for dataline in datalines:
        data += 'UPLOAD ' + data_owner.current_user + '_' \
            + str(k) + ' ' + dataline + '|'
        # upload data to proxy server. 
    data_owner.sentDatatoProxyServer(data)    
    print 'done!'

    # query generation. 
    query = data_owner.origquery_gen(k, n, 0.5) 
    querylines = query.split('\n')
    query = ''
    print 'Range Query test ...', 
    for queryline in querylines:
        query = 'QUERY ' + data_owner.current_user + '_' \
            + str(k) + ' ' + queryline + '|'
    data_owner.sentQuerytoProxyServer(query)
    print 'done!'

    stores = data_owner.getDataStores()
    print stores

    # data_owner.closeConnectiontoProxyServer()
