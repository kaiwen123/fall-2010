/* 
 * @file cfnode.h defining the cftree node class. 
 * @author simon guo <gsmsteve@gmail.com>
 * @date Thu May 26, 2011. 
 */
#ifndef _CFNode_H_
#define _CFNode_H_
#include "entry.h"
class Entry; 
class CFTree; 

using namespace std; 

/* The CFNode Class. */
class CFNode {
 private:
  map<int, Entry*> entries;	/* all entries in node. */
  CFTree* my_tree; 		/* tree pointer. */
  int level;			/* level of node. */
  int capacity; 		/* max number of entries. */

 public:
  CFNode(); 
  CFNode(CFTree* root);
  ~CFNode();

  // getters. 
  map<int, Entry*>& getEntries() {return entries;}
  Entry* getEntryById(int eid); 
  CFTree* getTree() {return my_tree;}
  //CFNode* getParent() {return parent;}
  int getNodeLevel() {return level;}
  bool isLeaf() {return (0 == level);}
  //bool isRoot() {return (NULL == parent);}
  bool isOverflow() {
    return (getEntries().size() >= capacity); 
  }

  // setters and content operations.
  //void setParent(CFNode* p) {parent = p;}
  void setLevel(int l) {level = 1;}
  //bool addChild(CFNode* c) {children.push_back(c);}
  //bool removeChild(CFNode* c); 
  bool addEntry(Entry* en); 
  bool removeEntry(Entry* en);

  // other funcs. 
  Entry* get_summary(); // get aggregated summary of node.
  int get_num_trans(); // how many trans in *this* node.
  bool containsEntry(int eid) {
    return (NULL != getEntryById(eid));
  }
  int add_trans(map<string, int>& trans); 
  int remove_trans(map<string, int>& trans, int eid); 

  // test trans for subtree selection. 
  float test_trans(map<string, int>& trans); 
  bool partition(CFNode* node);
  void absorb(Entry* en); 
  
  //float getSummaryWcd(); 
  void pprint(); 
  bool operator<(CFNode* node);
  friend ostream& operator<<(ostream& out, CFNode& node); 
};

#endif
