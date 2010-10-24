#include "DataServer.h"

bool DataServer::instanceFlag = false; 
DataServer* DataServer::instance = NULL;

// Constructor. 
DataServer::DataServer() {

}

// Destructor. 
DataServer::~DataServer() {
  instanceFlag = false; 
}

// get instance. 
DataServer* DataServer::getInstance() {
  if(!instance) {
    instance = new (std::nothrow) DataServer();
    instanceFlag = true; 
  }
  return instance; 
}

// Insert processing by server. 
string DataServer::doInsert(string dinsert) {
  list<double> data;
  parseParam(dinsert, data);
  // First, get the dimenstion. 
  int op = data.front(); data.pop_front();
  int id = data.front(); data.pop_front();
  int dim = data.front(); data.pop_front();

#ifdef DEBUG_INSERT_DATA
  cout << "Operation Code: " << op
       << " Data Id: " << id
       << " Dimension: " << dim << " " 
       << __FILE__ << ":" 
       << __LINE__ << endl; 
#endif
  
  // Second, check if store exists. 
  // if exists, do the insert directly. 
  // if not, create store before insert.
  if(exists(dim)) {
    //cout << "Store exists. ..." << endl;
    DataStore* store = stores[dim];
    double phigh[dim], plow[dim];
    
    int i = 0;
    while(data.size()) {
      plow[i] = data.front(); 
      data.pop_front();
      phigh[i] = data.front();
      data.pop_front();
      i++;
    }
    store->insertData(phigh, plow, dim, id); // test .
  } else {
    //cout << "Store doesn't exist. ..." << endl;
    // now create a store. 
    DataStore* store = new (nothrow) DataStore(dim);
    if(!store) {
      cerr << "Error Creating DataStore Object...." << endl; 
      return "FAILED";
    }
    stores[dim] = store;
  }
  // result response to the server. 
  return "Insert SUCCESS";
}

// check if store already exist.
bool DataServer::exists(int dim) {
  return stores[dim] == NULL ? false : true;
}
// Query processing by server. 
string DataServer::doQuery(string data) {
  return "Here is the query result";
}

// help function for parameter parsing.
// parsing parameters. with the form: 
// [dim d00 d01 d10 d11 ......]
bool DataServer::parseParam(string param, list<double>& data) {
  // data format: 
  // 1 0 5 1.0012 1.0012 .......
  char** p;
  double t_dat; 
  int pos;
  data.clear();
  do {
    pos = param.find_first_of(" ");
    if(param =="" || param == " " || pos <= 0) break;
    t_dat = strtod(param.substr(0, pos).c_str(), p);
    param = param.substr(pos+1, param.size()-pos).c_str();
    //cout << "param is: " << param << endl;
    data.push_back(t_dat);
  } while(1);
  return true;
}

