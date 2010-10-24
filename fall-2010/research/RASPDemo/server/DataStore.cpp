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
  cout << "Pre result is: " << preresult << endl;
  if(validate(preresult)) {	// passed validation.
    result_ += "Passed."; 
  }
}

// Validation implementation. 
bool StoreVisitor::validate(const string& s) {
  cout << "TODO: Now, validating the result......" << endl;
  return true;
}

////////////////////////////////////////////////////
// DataStore class. 
////////////////////////////////////////////////////

// Constructor 
DataStore::DataStore(int dim):dim_(dim){
  cout << "TODO: Creating data store object...." << endl;
  string baseName = '0' + dim + "_";
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

// Insert data into spatial index. 
bool DataStore::insertData(double *phigh, double *plow, int dim, int id) {
#ifdef DEBUG_INSERT_DATA
  cout << "Inserting data ... " 
       << __FILE__ << ":" 
       << __LINE__ << endl; 
#endif
  Region r = Region(plow, phigh, dim); 
  string data("DATA");
  index_->insertData(data.size() + 1,				
  		   reinterpret_cast<const byte*>(data.c_str()),
  		   r, id);
  return true; 
}

// Query data within this store. 
// In order to do query, I need to have, 
// One: region. 
// Two: visitor object. 
// Thr: intersect query.
string DataStore::queryData(double *phigh, double *plow, int dim) {
  cout << "Done: Do the querying...." << endl;
  Region r = Region(plow, phigh, dim);
  index_->intersectsWithQuery(r, visitor);
  return visitor.getQueryResult(); 
}

// flush data onto disk. 
// void DataStore::flush() {
//DiskStorageManager *x = dynamic_cast<DiskStorageManager*>(diskfile_);
// }
