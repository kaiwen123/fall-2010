#ifndef _CFTree_H_
#define _CFTree_H_
#include "entry.h"
#include <iostream>
#include <vector>

using namespace std; 

class Cftree {
 private: 
  // Entry and tree structure parameters.
  int maxtrans; 		/* maxnum of trans for leaf node. */
  int fanout;			/* maxnum of children for nonleaf nodes. */
  int maxlevel; 		/* the maximum height of cftree. */

  // summary about overall transactions.
  int g_nk; 			/* total transaction count. */
  int g_sk;			/* total item occurance count. */

  // tree elements. 
  Entry* cfroot;		/* root of the tree. */
  map<int, Entry*> allentries; 	/* hash table of all entries. */

 public:
  Cftree(int cap, int fo, int level);
  ~Cftree(); 
  
  // getters
  inline int getLeafMaxTrans() {return maxtrans;}
  inline int getFanout() {return fanout;} 
  inline int getMaxlevel() {return maxlevel;}
  inline int getGNk() {return g_nk;}
  inline int getGSk() {return g_sk;}
  
  // get operation for nodes/entries. 
  Entry* getNodeParent(int eid); 
  map<int, Entry*>& getNodeChildren(int eid);
  int getChildCount(int eid);
  bool isOverFlow(int eid); 
  
  // Operations related to the wcd algorithm. 
  int insert_trans(map<string, int>& trans);
  int adjust_trans(map<string, int>& trans, int oldeid, int neweid);
  int test_trans(map<string, int>& trans, int type, int member = -1);
  void traverse(Entry* c);

  // tree operations. 
  Entry* getEntryById(int eid) {return allentries[eid];}
  bool split(Entry* en); 
};

#endif
