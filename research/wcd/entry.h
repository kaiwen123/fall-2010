#ifndef _Entry_H_
#define _Entry_H_
#include <string>
#include <iostream>
#include <vector>
#include <map>
#include "config.h"
#include "libs.h"

using namespace std; 

class Entry {
 private:
  // entry summary information. 
  int eid; 			/* The entry id. */
  int sk; 			/* total item occurences. */
  int nk; 			/* total transactions. */
  float wcd; 			/* weighted clustering density. */
  int sk2; 			/* square sum of item occurences. */
  bool leaf; 			/* if this entry is a leaf node. */
  map<string, int> items; 	/* items summary in this entry. */
  
  static int e_counter;	     /* for generating globally unique eid. */
 public:
  Entry();
  ~Entry();

  // getters for class. 
  bool isLeaf() {return leaf;}
  int getEid() {return eid;}
  int getSk() {return sk;}
  int getNk() {return nk;}
  float getWcd() {return wcd;}
  int getSk2() {return sk2;}
  map<string, int>& getItems() {return items;}

  // core operations over the entry/cluster.
  int add_trans(map<string, int>& trans);
  int remove_trans(map<string, int>& trans);
  float test_trans(map<string, int>& trans, int type);

  // print and output operations.
  bool operator<(Entry* en){return (wcd < en->getWcd()); }
  friend ostream& operator<<(ostream& out, Entry& en);
  void pprint(); 
};

#endif
