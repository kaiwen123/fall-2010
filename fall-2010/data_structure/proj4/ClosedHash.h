// ClosedHash.h
//   This is the header for the abstract class defining a hash.
//   Your hash should derive from this class as follows:
//     class ClosedHash : public Hash
//
//   NOTE:  You may change this definition to hold any data
//          type you need.
//
// M. Raymer, 2/2007
// D. C. Wlodarski, modified 5/2010
// --------------------------------------------------------------
#ifndef __Closed_Hash_h__
#define __Closed_Hash_h__
#include <iostream>
#include "Hash.h"
using namespace std;

class ClosedHash : public Hash {
 protected: 
  static const int MAX_KEY = 999999;
  static const int NUM_SLOTS = 32768;

 private: 
  unsigned int HashTable[NUM_SLOTS];
  int cnt; 

 public:
  ClosedHash();
  ClosedHash(int size);
  virtual ~ClosedHash(){}
  bool insert(int newValue, int &collisions);
  bool find(int searchValue, int &probes) const;
  bool remove(int delValue);
  int count() const {return cnt;}
  float alpha() const {return count() / NUM_SLOTS;}
  bool full() const {return count() >= NUM_SLOTS;}
  bool isEmpty() {return cnt == 0;}
  unsigned int h(int key) const;
  unsigned int h2(int key) const;
};

#endif /* __Closed_Hash_h__ */
