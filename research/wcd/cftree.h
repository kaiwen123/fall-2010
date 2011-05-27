#ifndef _CFTree_H_
#define _CFTree_H_
#include "cfnode.h"

using namespace std; 

class CFTree {
 private: 
  // Node and tree structure parameters.
  int fanout;			/* max children for index nodes. */
  int maxlevel; 		/* max height of cftree. */
  int maxentry;			/* max number of entries in leaf. */
  int level;			/* current level of tree. */

  // summary about overall transactions.
  int g_nk; 			/* total transaction count. */
  int g_sk;			/* total item occurance count. */

  // tree elements. 
  CFNode* cfroot;		/* root of the tree. */

 public:
  CFTree(int fo, int maxentries, int level);
  CFTree(CFNode* node); 
  ~CFTree(); 

  // getters
  int getFanout() {return fanout;} 
  int getMaxlevel() {return maxlevel;}
  int getMaxEntry() {return maxentry;}
  int getGNk() {return g_nk;}
  int getGSk() {return g_sk;}
  CFNode* getRoot() {return cfroot;}
  
  // Operations related to the wcd algorithm. 
  int insert_trans(map<string, int>& trans);
  int adjust_trans(map<string, int>& trans, int eid);
  CFTree* choose_subtree(map<string, int>& trans, vector<CFNode*>& node);
  void traverse(CFNode* root);
  CFNode* findEntry(CFNode* node, int eid);

  // tree operations. 
  bool isOverFlow(CFNode* node);
  bool split(CFNode* node); 

  // output. 
  void pprint(); 
  friend ostream& operator<<(ostream& out, CFTree& cftree);
};

#endif
