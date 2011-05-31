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
  int uplimit;			/* max number of leaf nodes. */

  // summary about overall transactions.
  int g_nk; 			/* total transaction count. */
  int g_sk;			/* total item occurance count. */

  // tree elements. 
  CFNode* cfroot;		/* root of the tree. */
  vector<CFNode*> allnodes;	/* all cftree nodes. */

 public:
  CFTree(int fo, int maxentries, int level);
  ~CFTree(); 

  // getters
  int getFanout() {return fanout;} 
  int getMaxlevel() {return maxlevel;}
  int getMaxEntry() {return maxentry;}
  int getGNk() {return g_nk;}
  int getGSk() {return g_sk;}
  CFNode* getRoot() {return cfroot;}

  // setters.
  int setUplimit(); // number of max total nodes.
  
  // Operations related to the wcd algorithm. 
  int insert_trans(map<string, int>& trans);
  int adjust_trans(map<string, int>& trans, int eid);
  CFNode* choose_subtree(map<string, int>& trans, vector<CFNode*>& node);
  void traverse(CFNode* root);
  CFNode* findEntry(CFNode* node, int eid);

  // tree operations. 
  // bool isOverFlow(CFNode* node);
  bool isFull() {return allnodes.size() >= uplimit; }
  bool split(CFNode* node); 
  CFNode* findNode(); // might be deprecated. 
  CFNode* newNode();

  // output. 
  void pprint(); 
  friend ostream& operator<<(ostream& out, CFNode* root);
};

#endif
