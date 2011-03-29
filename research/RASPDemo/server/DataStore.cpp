#include "DataStore.h"

////////////////////////////////////////////////////
// StoreVisitor class. 
////////////////////////////////////////////////////
// StoreVisitor Implementation. 
StoreVisitor::StoreVisitor() {
  numNodeVisited = 0;
  totalResult = 0;
  preresult = 0;
  validate_key = ""; 
  result_ = "";
}

StoreVisitor::~StoreVisitor() {}

// Implementation of visitData. 
void StoreVisitor::visitData(const IData& d){
  byte* data; 
  size_t len = 0;
  d.getData(len, &data);
  string preresult = reinterpret_cast<char*>(data);
#ifdef DEBUG_QUERY_DATA
  cout << "Pre-Query Result: " << preresult << endl;
#endif
  if(validate(preresult)) {	// passed validation.
    result_ += preresult; 
  }
}

// Validation implementation. 
bool StoreVisitor::validate(const string& s) {
#ifdef DEBUG_QUERYD_DATA
  cout << "TODO: Now, validating the result......" << endl;
#endif
  // get the validation key. 

  // do the validation.
  int k = 5; 
  float result = 0.0, x[k];
  float y[k], T[k];
  for(int i = 0; i < k; i++){
    x[i] = 0.0; 
  }
  for(int i = 0; i < k; i++){
    for(int j = 0; j < k; j++) {
      x[i] += y[j] * T[k*j + i]; 
    }
    result += x[i] * y[i]; 
  }
  // if result >= 0 then pass, else fail.
  return true;
}

////////////////////////////////////////////////////
// DataStore class. 
////////////////////////////////////////////////////

// Constructor 
DataStore::DataStore(int dim):dim_(dim){
  char tmp[3]; 
  sprintf(tmp, "%d", dim);
  string baseName =  "db_" + string(tmp);
  cout << "Creating data store with basename : " << baseName << endl;
  // disk file. 
  diskfile_ = createNewDiskStorageManager(baseName, 4096);
  // buffer in memory. 
  buffer_ = createNewRandomEvictionsBuffer(*diskfile_, 10, false);
  // index. 
  int capacity = 100;
  id_type indexIdentifier;
  index_ = createNewRTree(*buffer_, 0.7, capacity, capacity, dim, \
			      RV_RSTAR, indexIdentifier);
}

// Destructor. 
DataStore::~DataStore() {cout << "Destroying data store." << endl;}

// Insert data into spatial index. 
// dstr is the string representation of the data. 
bool DataStore::insertData(double *phigh, double *plow, int dim, int
			   id, string dstr) {
#ifdef DEBUG_INSERT_DATA
  cout << "Inserting data ... " 
       << dstr 
       << __FILE__ << ":" 
       << __LINE__ << endl; 
#endif
  Region r = Region(plow, phigh, dim); 
  index_->insertData(dstr.size() + 1,				
  		   reinterpret_cast<const byte*>(dstr.c_str()),
  		   r, id);
  return index_->isIndexValid();
}

// Query data within this store. 
string DataStore::queryData(double *phigh, double *plow, int dim) {
#ifdef DEBUG_QUERYD_DATA
  cout << "Do the querying...." << endl;
#endif
  Region r = Region(plow, phigh, dim);
  index_->intersectsWithQuery(r, visitor);
  return visitor.getQueryResult(); 
}
