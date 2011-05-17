#ifndef _CTree_H_
#define _CTree_H_
#include "entry.h"
#include <iostream>
#include <vector>

using namespace std; 

class Ctree {
 private: 
  // Entry and tree structure parameters.
  int maxtrans; 		/* max number of trans for leaf node. */
  int fanout;			/* maximum number of children for nonleaf nodes. */
  int maxlevel; 		/* the maximum height of cftree. */

  // tree elements. 
  Entry* root;			/* root of the tree. */
  vector<Entry*> allclusters; 	/* pointers of all entries. */
  map<int, Entry*> clusters; 	/* hash table of all entries. */

 public:
  Cftree(int cap, int fo, int level);
  ~Ctree(); 
  
  // getters
  inline int getLeafMaxTrans() {return maxtrans;}
  inline int getFanout() {return fanout;} 
  inline int getMaxlevel() {return maxlevel;}
  inline int getNodesize() {return _node_size;}
  inline int getRecordCount() {return record_count;}
  
  // Operations related to the wcd algorithm. 
  int insert_trans(map<string, int>& trans, int eid);
  int adjust_trans(map<string, int>& trans, int oldeid, int neweid);
  int test_trans(map<string, int>& trans, int type, int eid);
  void traverse(Entry* c);

  // tree operations. 
  Entry* getEntryById(int eid);
};

#endif
