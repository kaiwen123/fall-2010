#include "ClosedHash.h"

// Constructor of ClosedHash class. 
// It is responsible for initializing each slot of the HashTable array
// to EMPTY and the item count in the hash table to zero. 
ClosedHash::ClosedHash(int size):cnt(0),Hash() {
  for(int i = 0; i < size; i++){
    int empty = EMPTY; 
    HashTable.push_back(empty);
  }
}
// Insert an integer into the hash.  Return true if successful,
// false if there are no more slots in the hash, or the integer
// is already found. Store the number of collisions in the
// variable 'collisions'.
bool ClosedHash::insert(int newValue, int &collisions){
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
  int pidx = h2(newValue); 	// probe index; 

  for(int i = 0; ; i++) {
    int nidx = (hidx + i * pidx) % size(); 
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
// One possible bug here is, when I fill the hash table to be FULL,
// then I delete some items, in this case, the hash table items are
// either set to TOMBSTONE or filled. In such a case when I try to
// search an not-existed item, it will crash(maybe infinite loop). 
// My solution is to constrain the loop size to a very large
// number. This has been commented out. Make comments if you have any
// other work arounds. 
bool ClosedHash::find(int searchValue, int &probes) const{
  probes = 0;
  int hidx = h(searchValue); 	// home index; 
  int pidx = h2(searchValue); 	// probe index; 

  for(int i = 0; /*i < 1000000*/; i++) {
    //if(i < 0) return false;
    int nidx = (hidx + i * pidx) % size(); 
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
bool ClosedHash::remove(int delValue){
  if(isEmpty()) return false;
  int hidx = h(delValue); 	// home index; 
  int pidx = h2(delValue); 	// probe index; 

  for(int i = 0; ; i++) {
    int nidx = (hidx + i * pidx) % size(); 
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
unsigned int ClosedHash::h(int key) const {
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
  // or constant in class ClosedHash called maxSize.  This
  // is the value M (the number of buckets in the hash).  This
  // can be a private data element or constant.
  int maxSize = size();
  return hash % maxSize;
}

//  ****
//  For comments, see h(), above!
//  Also see the comments near the end of h2().
//  ****
unsigned int ClosedHash::h2(int key) const {
  unsigned int hash = 0;
  unsigned int keyarr = key;
  unsigned char *keyptr = (unsigned char *) &keyarr;
     
  for (int i = 0; i < sizeof(int); i++) {
    hash += keyptr[i];
    hash += (hash << 9);
    hash ^= (hash >> 5);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);

  // The following code ensures that the value of h2(k) will be
  // odd, which should be coprime with the size of the closed
  // hash (32,768 == 2^15).
  int maxSize = size();
  return (((hash * 2) + 1) % maxSize);
}
