// -*- C -*-
/**
 * @file DataServer.h
 * @brief Definition of DataServer class. 
 * Data server is responsible for managing Data Store objects.
 *
 * What data server can do: 
 * 0, Receive requests from clients on data storage and query. 
 * 1, Creation of DataStore object. 
 * 2, Deletion of DataStore object. 
 * 3, Managing the location of data for each DataStore object. 
 * 4, Validation, backup and so on... 
 * 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#ifndef _DATA_SERVER_H_
#define _DATA_SERVER_H_ 

#include <iostream>
#include <map>		     /* for data store objs. */
#include <queue>	     /* message queues. */
#include "DataStore.h"
#include "RequestMsg.h"
#include "ResponseMsg.h"

using namespace std;

class DataServer{
 private: 
  queue<RequestMsg> _requests;	 /* Requests. */
  queue<ResponseMsg> _responses; /* Responses. */
  map<int, DataStore*> _stores;	 /* DataStore objects. */

  /* Statistics about server. */
  // TODO

 public:
  DataServer(); 
  ~DataServer() {}
  
  /* getters. */
  int getNumStores() const {return _stores.size();}
  int getNumRequests() const {return _requests.size();}
  int getNumResponses() const {return _responses.size();}
  
  /* setters. */

  // Message collectors. 
  bool collectRequest(); 
  bool collectResponse();

  /**
   * @breif Request processor. 
   * This function is responsible for processing all the keys stored
   * in the request queue. If there should be a response to client,
   * this processor will create a response message and put the message
   * into the response queue which will be processed by the response
   * processor. 
   *
   * @param key The key(dimension) of the store to be created. 
   * @return true on success and false on failure. 
   */
  bool processRequest();

  /**
   * @breif Response processor. 
   * This function is responsible for processing all the keys stored
   * in the response queue. 
   *
   * @param key The key(dimension) of the store to be created. 
   * @return true on success and false on failure. 
   */
  bool processResponse();

 private: 
  /* A bunch of private store operators. */
  /**
   * @breif Creating the data store object with key. 
   * @param key The key(dimension) of the store to be created. 
   * @return true on success and false on failure. 
   */
  bool createStore(int key);

  /**
   * @breif Clear the content of a store. 
   * @param key The key(dimension) of the store to be cleared. 
   * @return true on success and false on failure. 
   */
  bool clearStore(int key);

  /**
   * @breif Loading store into memory. 
   * @param key The key(dimension) of the store to be loaded. 
   * @return true on success and false on failure. 
   */
  bool loadStore(int key);

};

#endif //ifdef
