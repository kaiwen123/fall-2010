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
string DataServer::doInsert(string data) {
  // First, get the dimenstion. 

  // Second, check if store exists. 
  // if exists, do the insert directly. 
  // if not, create store before insert.

  // result response to the server. 
}

// Query processing by server. 
string DataServer::doQuery(string data) {

}

// help function for parameter parsing.
// parsing parameters. with the form: 
// [dim d00 d01 d10 d11 ......]
void DataServer::parseParam(string &param) {
  // cout << "Parameters : " << str << endl; 
  char** p;
  double t_dat; 
  int pos;
  pos = param.find_first_of(" ");
  try {
    dim_ = atoi(param.substr(0, pos).c_str());
    cout << "Dimension is: " << dim_ << endl; 
    param = param.substr(pos+1, param.size() - pos);
  } catch(...) {
    cerr << "Error while parsing dimension." << endl;
  }

  // now, data parsing. 
  do {
    pos = param.find_first_of(" ");
    t_dat = strtod(param.substr(0, pos).c_str(), p);
    param = param.substr(pos+1, param.size()-pos).c_str();
    cout << "Data : " << t_dat << endl;
    data_.push_back(t_dat);
  } while(pos > 0);
}

