/* 
 * @file entry.h definition of the entry class.
 * @author simon guo <gsmsteve@gmail.com>
 * @date Tuesday, June 07, 2011. 
 */
#ifndef _Entry_H_
#define _Entry_H_

#include <string>
#include <iostream>
#include <vector>
#include <map>
#include "config.h"
#include "libs.h"

class CFNode; 
//class CFTree; 
using namespace std; 

class Entry {
 private:
  // entry summary information. 
  int eid; 			/* The entry id. */
  int sk; 			/* total item occurences. */
  int nk; 			/* total transactions. */
  float wcd; 			/* weighted clustering density. */
  int sk2; 			/* square sum of item occurences. */

  // string is the transaction element. 
  // int is the occurrence of the element within the cluster. 
  map<string, int> items;

  // structure linkage in the tree. 
  CFNode *child_ptr; 
  
  static int e_counter;	     /* for generating globally unique eid. */
 public:
  Entry();
  ~Entry();

  // getters for class. 
  int getEid() {return eid;}
  int getSk() {return sk;}
  int getNk() {return nk;}
  float getWcd() {return wcd;}
  int getSk2() {return sk2;}
  const map<string, int>& getItems() {return items;}

  // Structure operations.
  void del_child(); // release child of of *this* entry.
  CFNode* get_child() {return child_ptr;}
  void set_child(CFNode* node) {child_ptr = node; }

  // core operations over the entry/cluster.
  int add_trans(map<string, int>& trans);
  int remove_trans(map<string, int>& trans);
  float test_trans(map<string, int>& trans, t_type type);

  // print and output operations.
  bool operator> (Entry& en) {return (getWcd() > en.getWcd());}
  friend ostream& operator<<(ostream& out, Entry& en);
  bool operator == (Entry& en);
  Entry& operator+=(Entry& en);
  void pprint(); 
};
bool operator< (Entry lhs, Entry rhs); 
#endif
