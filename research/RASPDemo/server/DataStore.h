// -*- C -*-
/**
 * @file DataStore.h
 * @brief Definition of DataStore class. 
 * This function is responsible for storing data from client into
 * related dimension. Here related means the dimension of data, for
 * different dimensions of data we need to setup a separate database. 
 * Additionally, this function should provide a way to efficiently
 * handle data between memory and disk. 
 * 
 * Naming conventions: we will name database files as follows: 
 * "database-[dim]" dim is the dimension of data. 
 * 
 * @param data is the data sent by client, we need first to verify if
 * the format of data conforms to our designated format before doing
 * work that follows. 
 * The format of data is(sample): "2 223.123 223.123 34.23 34.23"
 * data fields are separated by space.
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#ifndef _DATA_STORE_H_
#define _DATA_STORE_H_ 
#include <gsl/gsl_blas.h>
#include <SpatialIndex.h>
#include <string> 
#include <stdlib.h>
#include <iostream>

using namespace SpatialIndex;
using namespace SpatialIndex::StorageManager;
using namespace SpatialIndex::RTree;
using namespace std;

/* A visitor class for visiting the data store. */
class StoreVisitor : public IVisitor {
 private:
  int numNodeVisited;		/* number of nodes visited. */
  int totalResult;		/* number of result after step two; */
  int preresult;		/* number of result after step one; */

  string validate_key; 		/* validator key. */
  string result_; 		/* The query result. */

 public:
  StoreVisitor();
  /* virtual functions. */
  virtual ~StoreVisitor();

  /**
   * @brief Data visitor. Not Implemented yet.
   * @param v A vector with IData to be visited. 
   * @return void. 
   */
  virtual void visitData(std::vector<const IData*>& v) {}

  /**
   * @brief Data visitor.
   * If a node is visited, it means that it will be part of the
   * preresult. Then it should be validated using the validationg key. 
   * TODO: validationg using the validation key.
   * @param d IData object to be visited. 
   * @return void. 
   */  
  virtual void visitData(const IData& d);
  
  /**
   * @brief Node visitor. In this implementation, it is only used to
   * count how many nodes are visited.
   * @param n node to be visited. 
   * @return void. 
   */
  virtual void visitNode(const INode& n) {numNodeVisited++;}

  /* getters. */
  string getQueryResult() const {return result_;}
  int getNumNodeVisited() { return numNodeVisited;}
  int getNumTotalResult() { return totalResult;}
  int getNumPreresult() {return preresult;}

  /* validation function. */
  bool validate(const string& s);
};

// The DataStore class.
// This class is responsible for creating, managing data stores. 
class DataStore{
 private: 
  int dim_; 			/* dimension. */
  IStorageManager *diskfile_;	/* disk file. */
  IBuffer *buffer_;		/* buffer. */
  ISpatialIndex* index_;	/* Spatial index file. */
  StoreVisitor visitor; 	/* Store visitor for query purpose. */

 public:
  /* constructors and destructor. */
  DataStore(int dim);
  ~DataStore();

  /* getters. */
  int getDimension() const {return dim_;};
  int getStoreSize(); 		/* how many data items are stored? */

  /* setters. */
  void setDimension(int d) {dim_ = d;}

  /**
   * @brief Do data query to this store. 
   * @param phigh The higher boundary of the query. 
   * @param plow The lower boundary of the query. 
   * @param dim The dimension of the query. 
   * @param id of the data to be inserted. 
   * @param dstr string of data.
   * @return result in string format (Need to be defined.).
   */
  bool insertData(double *phigh, double *plow, int dim, 
		  int id, string dstr); 

  /**
   * @brief Do data query to this store. 
   * @param phigh The higher boundary of the query. 
   * @param plow The lower boundary of the query. 
   * @param dim The dimension of the query. 
   * @return result in string format (Need to be defined.).
   */
  string queryData(double *phigh, double *plow, int dim); 

  /**
   * @brief Flush data onto disk file.
   * So the memory cache will be all purged.
   * @param none. 
   * @return void. 
   */
  void flush();
};

#endif //ifdef
