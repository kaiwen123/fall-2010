#include "RandHash.h"

// Constructor of RandHash class. 
// It is responsible for initializing each slot of the HashTable array
// to EMPTY and the item count in the hash table to zero. 
RandHash::RandHash(int size):cnt(0),Hash() {
  for(int i = 0; i < size; i++){
    int empty = EMPTY; 
    HashTable.push_back(empty);
    if(i > 0) {
      RandPosTable.push_back(i);
    }
  }
  random_shuffle(RandPosTable.begin(), RandPosTable.end());
}
// Insert an integer into the hash.  Return true if successful,
// false if there are no more slots in the hash, or the integer
// is already found. Store the number of collisions in the
// variable 'collisions'.
bool RandHash::insert(int newValue, int &collisions){
  collisions = 0;
  // In order for find() to work properly, let's only insert size()-1
  // items into the hashtable. 
  if(full()) {
    cerr << "ERROR! Hash Table is Full!" << endl;
    return false; 
  }
  if(find(newValue, collisions)) return false; 
  collisions = 0;
  int hidx = h(newValue); 	// home index; 

  for(int i = 0; i < size(); i++) {
    int nidx;
    if(i == 0) {
      nidx = hidx % size();
    } else {
      nidx = (hidx + RandPosTable[i]) % size(); 
    }
    if(HashTable[nidx] == EMPTY || HashTable[nidx] == TOMBSTONE) {
      HashTable[nidx] = newValue;
      cnt++;
      return true; 
    } else {
      collisions++;
    }
  }
  return false; 
}

// Find an integer value in the hash.  Return true if found,
// false if not.  Store the number of probes in the variable
// 'probes'.
bool RandHash::find(int searchValue, int &probes) const{
  probes = 0;
  int hidx = h(searchValue); 	// home index; 

  for(int i = 0; i < size(); i++) {
    int nidx;
    if(i == 0) {		// home location
      nidx = hidx % size();
    } else {			// probing.
      nidx = (hidx + RandPosTable[i]) % size(); 
    }
    if(HashTable[nidx] == searchValue) { // get it.
      return true;
    } else { 			// probe or can't find.
      probes++;
      if(HashTable[nidx] == EMPTY) {
	return false; 
      }
    }
  }
  return false;
}

// Delete an integer from the hash.  Return true if successful,
// false if unsuccessful.
bool RandHash::remove(int delValue){
  if(isEmpty()) return false;
  int hidx = h(delValue); 	// home index; 

  for(int i = 0; i < size(); i++) {
    int nidx;
    if(i == 0) {		// home location
      nidx = hidx % size();
    } else {			// probing.
      nidx = (hidx + RandPosTable[i]) % size(); 
    }
    if(HashTable[nidx] == delValue) { // found and delete. 
      HashTable[nidx] = TOMBSTONE;
      cnt--;
      return true;
    }
    if(HashTable[nidx] == EMPTY) {
      return false; 		// can't find value. 
    }
  }
  return false;
}

////////////////////////////////////////////////////
// Hash functions. 
////////////////////////////////////////////////////
unsigned int RandHash::h(int key) const {
  unsigned int hash = 0;      // The hash value starts at 0
  unsigned int keyarr = key;  // A copy of the key

  // We will treat the key (an integer) as an array of 4
  // unsigned characters
  unsigned char *keyptr = (unsigned char *) &keyarr;
     
  // Mix each 8-bit character into the hash
  for (int i = 0; i < (sizeof(int)); i++) {

    // This is the combining step:
    hash += keyptr[i];

    // This is the mixing step:
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }

  // After all the bits of the key have been mixed in,
  // ensure that they are properly distributed throughout
  // the final hash value:
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);

  // This last line assumes the you have a data member 
  // or constant in class RandHash called maxSize.  This
  // is the value M (the number of buckets in the hash).  This
  // can be a private data element or constant.
  int maxSize = size();
  return hash % maxSize;
}
