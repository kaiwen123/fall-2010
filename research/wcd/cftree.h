#ifndef _CFTree_H_
#define _CFTree_H_ 1
#include "cfnode.h"

using namespace std; 

class CFTree {
 private: 
  // Node and tree structure parameters.
  // int num_nodes;		/* number of nodes in tree. */

  // tree elements. 
  CFNode* cfroot;		/* root of the tree. */

 public:
  CFTree(); 
  ~CFTree(); 

  // getters
  // int getNumNodes() {return num_nodes;}
  CFNode* getRoot() {return cfroot;}
  
  // Operations related to the wcd algorithm. 
  void insert_trans(map<string, int>& trans);
  bool absort_trans(map<string, int>& trans); 
  bool traverse(CFNode* root);
  CFNode* findEntry(int eid);

  // tree operations. 
  bool isFull() {return getNumNodes() >= getUplimit(); }
  CFNode* findNode(); // might be deprecated. 
  // CFNode* newNode();

  // output. 
  void pprint(); 
};

#endif
