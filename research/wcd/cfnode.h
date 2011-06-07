/* 
 * @file cfnode.h defining the cftree node class. 
 * @author simon guo <gsmsteve@gmail.com>
 * @date Thu May 26, 2011. 
 */
#ifndef _CFNode_H_
#define _CFNode_H_
#include "entry.h"
#include "cftree.h"
#include "wcd.h"

//class Entry; 
class CFTree; 
//class WCD;

using namespace std; 

/* The CFNode Class. */
class CFNode {
 private:
  map<int, Entry*> entries;	/* all entries in node. */
  CFTree* my_tree; 		/* tree pointer. */
  int level;			/* level of node. */
  int capacity;			/* max entries in node. */

 public:
  CFNode(CFTree* tree, int cap);
  ~CFNode();

  // getters. 
  map<int, Entry*>& getEntries() {return entries;}
  int getEntryCount() {return entries.size();}
  Entry* getEntryById(int eid); 
  CFTree* getTree() {return my_tree;}
  int getLevel() {return level;}
  bool isLeaf() {return (0 == level);}
  bool isOverflow() {
    return (getEntries().size() > capacity); 
  } 

  // setters and content operations.
  void setLevel(int l) {level = l;}
  bool addEntry(Entry* en); 
  bool removeEntry(Entry* en);

  // other funcs. 
  Entry* get_summary(); // get aggregated summary of node.
  int get_num_trans(); // how many trans in *this* node.
  bool containsEntry(int eid) {
    return (NULL != getEntryById(eid));
  }
  Entry* newEntry(); 
  Entry* choose_subtree(map<string, int>& trans);
  int insert_trans(map<string, int>& trans, CFNode** child); 
  int remove_trans(map<string, int>& trans, int eid); 

  // test trans for subtree selection. 
  float test_trans(map<string, int>& trans); 

  // node split and partitioning. 
  bool partition(CFNode* node);
  void absorb(Entry* en); 
  
  //float getSummaryWcd(); 
  void pprint(); 
  bool operator<(CFNode* node);
  friend ostream& operator<<(ostream& out, CFNode& node); 
};

#endif
