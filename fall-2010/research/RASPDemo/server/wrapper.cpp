/* File : time.c */
#include "DataServer.h"

using namespace std;

// Server object. 
DataServer *server = DataServer::getInstance();	

// Insert data into data store. 
// A successful message will be returned on success. 
// In case of failure, a Failed message will be returned. 
string insertData(string data) {
  return server->doInsert(data);
}

// Query data from data store. 
string queryData(string data) {
  return server->doQuery(data);
}
