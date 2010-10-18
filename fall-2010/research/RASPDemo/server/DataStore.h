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

#include <SpatialIndex.h>
#include <string> 
#include <queue>		// queue to store data. 
#include <stdlib.h>
#include <iostream>

using namespace SpatialIndex;
using namespace std;

class DataStore{
 private: 
  int _dim; 			/* dimension. */
  ISpatialIndex* _data;		/* Spatial index for data storage. */

 public:
  /* constructors and destructor. */
  DataStore(); 
  DataStore(int dim);
  ~DataStore(){}

  /* getters. */
  int getDimension() const {return _dim;};
  int getStoreSize(); 		/* how many data items are stored? */

  /* setters. */
  void setDimension(int d) {_dim = d;}

  /**
   * @brief Insert data item into store. 
   * @param data to be inserted. 
   * @return true on success, false on failure.
   */
  bool insertData(string data);

  /**
   * @brief Do data query to this store. 
   * @param query. 
   * @return result in string format (Need to be defined.).
   */
  string queryData(); 

  /**
   * @brief Clean everything within the store. 
   * @param none. 
   * @return void. 
   */
  bool dumpData(); 

  /**
   * @brief Clean everything within the store. 
   * @param none. 
   * @return void. 
   */
  void purgeStore(); 
};

#endif //ifdef
