#ifndef _CFNode_H_
#define _CFNode_H_
#include "entry.h"

using namespace std; 

/* The CFNode Class. */
class CFNode {
 private:
  map<int, Entry*> entries;	/* all entries in node. */
  vector<CFNode*> children; 	/* children of node. */
  CFNode* parent; 		/* the parent node. */
  int level;			/* level of node. */

 public:
  CFNode(); 
  ~CFNode();
  // getters. 
  vector<CFNode*>& getChildren() {return children;} 
  map<int, Entry*>& getEntries() {return entries;}
  Entry* getEntryById(int eid); 
  CFNode* getParent() {return parent;}
  int getNodeLevel() {return level;}
  bool isLeaf() {return (0 == children.size());}
  bool isRoot() {return (NULL == parent);}
  bool isIndexOverflow(int maxfanout) {
    return (children.size() > maxfanout); 
  }
  bool isLeafOverflow(int maxentries) {
    return (entries.size() > maxentries);
  }

  // setters and content operations.
  void setParent(CFNode* p) {parent = p;}
  void setLevel(int l) {level = 1;}
  bool addChild(CFNode* c) {children.push_back(c);}
  bool removeChild(CFNode* c); 
  bool addEntry(Entry* en); 
  bool removeEntry(Entry* en);

  // other funcs. 
  bool containsEntry(Entry *en) {
    return (NULL != getEntryById(en->getEid()));
  }
  int add_trans(map<string, int>& trans); 
  int remove_trans(map<string, int>& trans, int eid); 

  // test trans for subtree selection. 
  float test_trans(map<string, int>& trans); 
  bool partition(CFNode* node);
  float getSummaryWcd(); 
  bool operator<(CFNode* node);
  friend ostream& operator<<(ostream& out, CFNode& node); 
};

#endif
