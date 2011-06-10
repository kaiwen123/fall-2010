#ifndef _CFTree_H_
#define _CFTree_H_
#include "cfnode.h"

//class Entry;
using namespace std; 

class CFTree {
 private: 
  // Node and tree structure parameters.
  //int maxentry;			/* max number of entries in leaf. */
  // int uplimit;			/* max number of leaf nodes. */

  int num_nodes;		/* number of nodes in tree. */

  // tree elements. 
  CFNode* cfroot;		/* root of the tree. */
  
  //vector<CFNode*> allnodes;	/* all cftree nodes. */

 public:
  //CFTree(int fo, int maxlvl);
  CFTree(); 
  ~CFTree(); 

  // getters
  //int getMaxEntry() {return maxentry;}
  int getNumNodes() {return num_nodes;}
  CFNode* getRoot() {return cfroot;}

  // setters.
  //int setUplimit(int fo, int maxlvl); // number of max total nodes.
  
  // Operations related to the wcd algorithm. 
  void insert_trans(map<string, int>& trans);
  //int adjust_trans(map<string, int>& trans, int eid);
  bool absort_trans(map<string, int>& trans); 
  void traverse(CFNode* root);
  CFNode* findEntry(int eid);

  // tree operations. 
  bool isFull() {return num_nodes >= getUplimit(); }
  CFNode* findNode(); // might be deprecated. 
  CFNode* newNode();

  // output. 
  void pprint(); 
};

#endif
