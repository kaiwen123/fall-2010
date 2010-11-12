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
#ifndef __Rand_Hash_h__
#define __Rand_Hash_h__
#include <iostream>
#include <vector>
#include <fstream>
#include <algorithm>
#include "Hash.h"
using namespace std;

class RandHash : public Hash {
 private: 
  vector<int> HashTable;
  /* random permutation of the hash table positions */
  vector<int> RandPosTable;
  int cnt; 

 public:
  RandHash(int size);
  ~RandHash(){HashTable.clear();}
  bool insert(int newValue, int &collisions);
  bool find(int searchValue, int &probes) const;
  bool remove(int delValue);
  int count() const {return cnt;}
  int size() const {return HashTable.size();}
  float alpha() const {return (float)count() / size();}
  bool full() const {return count() == size() - 1;}
  bool isEmpty() {return cnt == 0;}
  unsigned int h(int key) const;
};

#endif /* __Rand_Hash_h__ */
