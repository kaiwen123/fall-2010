#ifndef _Entry_H_
#define _Entry_H_
#include <string>
#include <iostream>
#include <vector>
#include <map>
#include "config.h"
#include "libs.h"

class CFNode; 
class CFTree; 
using namespace std; 

class Entry {
 private:
  // entry summary information. 
  int eid; 			/* The entry id. */
  int sk; 			/* total item occurences. */
  int nk; 			/* total transactions. */
  float wcd; 			/* weighted clustering density. */
  int sk2; 			/* square sum of item occurences. */
  int level; 			/* level of entry. */
  map<string, int> items; 	/* items summary in this entry. */

  // structure linkage in the tree. 
  CFTree *my_tree; 
  CFNode *child_ptr; 
  
  static int e_counter;	     /* for generating globally unique eid. */
 public:
  Entry();
  Entry(CFTree* root);
  ~Entry();

  // getters for class. 
  bool isLeaf() {return (0 == level);}
  int getEid() {return eid;}
  int getSk() {return sk;}
  int getNk() {return nk;}
  float getWcd() {return wcd;}
  int getSk2() {return sk2;}
  int getLevel() {return level;}
  map<string, int>& getItems() {return items;}

  // Structure operations.
  void del_child(); // release child of of *this* entry.
  CFNode* get_child() {return child_ptr;}
  CFTree* get_tree() {return my_tree;} 

  // core operations over the entry/cluster.
  int add_trans(map<string, int>& trans);
  int remove_trans(map<string, int>& trans);
  float test_trans(map<string, int>& trans, int type);

  // print and output operations.
  bool operator < (Entry& en){return (getWcd() < en.getWcd()); }
  friend ostream& operator<<(ostream& out, Entry& en);
  bool operator == (Entry& en);
  Entry& operator+=(Entry& en);
  Entry& operator=(Entry& en);
  void pprint(); 
};

#endif
