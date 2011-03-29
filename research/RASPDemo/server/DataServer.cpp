#include "DataServer.h"

bool DataServer::instanceFlag = false; 
DataServer* DataServer::instance = NULL;

// Constructor. 
DataServer::DataServer() {}

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
       << " Dimension: " << dim << " ->AT: " 
       << __FILE__ << ":" 
       << __LINE__ << endl; 
#endif
  
  // Second, check if store exists. 
  // if exists, do the insert directly. 
  // if not, create store before insert.
  if(exists(dim)) {
    DataStore* store = stores[dim];
    double phigh[dim], plow[dim];
    
    int i = 0;
    while(data.size()) {
      plow[i] = data.front(); 
      data.pop_front();
      phigh[i] = data.front();
      data.pop_front();
#ifdef DEBUG_INSERT_DATA
      cout << plow[i] << " " << phigh[i] << endl;
#endif
      i++;
    }
#ifdef DEBUG_INSERT_DATA
    cout << "using Data Store " << store 
	 << " with dimension: " << dim << endl;
#endif
    store->insertData(phigh, plow, dim, id, dinsert);
  } else {
    DataStore* store = new (nothrow) DataStore(dim);
    if(!store) {
      cerr << "Error Creating DataStore Object...." << endl; 
      return "FAILED";
    }
    stores[dim] = store;
  }
  char tmp[20];
  sprintf(tmp, "Data %d With Dimension %d", id, dim);
  // result response to the server. 
  return "Upload " + string(tmp) + " SUCCESSFUL!";
}

// check if store already exist.
bool DataServer::exists(int dim) {
  return stores[dim] == NULL ? false : true;
}

// Query processing by server. 
string DataServer::doQuery(string query) {
  list<double> data;
  parseParam(query, data);
  // First, get the dimenstion. 
  int op = data.front(); data.pop_front();
  int id = data.front(); data.pop_front();
  int dim = data.front(); data.pop_front();

#ifdef DEBUG_PARSING_DATA
  cout << "Operation Code: " << op
       << " Data Id: " << id
       << " Dimension: " << dim << " ->AT: " 
       << __FILE__ << ":" 
       << __LINE__ << endl; 
#endif

  if(exists(dim)) {
    DataStore* store = stores[dim];
    double phigh[dim], plow[dim]; 
    int i = 0;
    while(data.size()) {
      plow[i] = data.front(); 
      data.pop_front();
      phigh[i] = data.front();
      data.pop_front();
      i++;
    } // while
    return "Result: " + store->queryData(phigh, plow, dim);
  } else {
    return "No DATA in Server.";
  } // if-else
}

// Parsing string parameter to vector. 
bool DataServer::parseParam(string param, list<double>& data) {
  // data format: 1 0 5 1.0012 1.0012 .......
  char** p;
  double t_dat; 
  int pos;
  data.clear();
  do {
    pos = param.find_first_of(" ");
    if(param =="" || param == " ") break;
    t_dat = strtod(param.substr(0, pos).c_str(), p);
    param = param.substr(pos+1, param.size()-pos).c_str();
#ifdef DEBUG_PARSING_DATA
    cout << "Parsing: " << t_dat 
	 << "\tstring: " << param
	 << endl;
#endif
    data.push_back(t_dat);
  } while(pos > 0);
  return true;
}

// Delete all the data stores. 
bool DataServer::deleteStores() {
  map<int, DataStore*>::iterator it; 
  for(it = stores.begin(); it != stores.end(); it++) {
    if((*it).second) delete (*it).second;
  }
}
