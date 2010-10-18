#include "DataStore.h"

bool DataStore::insertData(string data) {
  string dstore=data; 		// Store data for later use.
  // parse data;
  double d_tmp;			// temporary data; 
  queue<double> q_data; 	// data vector.
  char** p; 			// used for operation strtod.
  while(1) {
    int posspace = data.find(' ',0);
    if(posspace!=string::npos) {
      d_tmp=strtod(data.substr(0, posspace).c_str(), p);
      q_data.push(d_tmp);
      data=data.substr(posspace+1,data.size()-posspace).c_str(); 
      cout << d_tmp << " " << data << endl; 
    } else break;
  }
  int dim = (int)q_data.front(); 

  // validate data; 
  if(q_data.size() != 2 * dim) {
    cerr << "Input data corrupt..." << endl; 
    return 1; 
  }

  // insert data into spatial index database; 
  double plow[dim], phigh[dim];	// low and high point of region. 
  int i = 0; 
  while(!q_data.empty()){
    plow[i] = q_data.front();
    phigh[i] = q_data.front(); 
    i++;
  }
  Region r = Region(plow, phigh, dim); 
  
  // tree->insertData(data.size() + 1,				
  // 		   reinterpret_cast<const byte*>(data.c_str()),
  // 		   r, id);
  return true; 
}

// Query data within this store. 
string DataStore::queryData() {

}

// Dump all the data of current store. 
bool DataStore::dumpData() {

}

// Purge Current store. 
void purgeStore() {

}
