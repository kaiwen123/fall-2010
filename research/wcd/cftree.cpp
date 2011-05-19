#include "cftree.h"
// ------------------------------------------------------------
// @file cftree.cpp Implementation of the CFTree structure. 
// @func constructor. 
// @func insert_trans - transaction insertion. 
// @func adjust_trans - adjustment of transactions.
// @func choose_subtree - find the best entry to insert trans into. 
// @func print, output etc. 
// ------------------------------------------------------------
// 
// @brief CFTree Constructor and distructor.
// @param fo fanout of nonleaf node. 
// @param level maximum level of tree structure. 
// @return None, constructor no return.
CFTree::CFTree(int fo, int maxentries, int level) {
  fanout = fo; 
  maxentry = maxentries; 
  maxlevel = level; 
  level = 1; 			// only one root node. 

  // Initialize the tree with two entries on level 2.   
  CFNode* root = new CFNode(); 

  // set root pointer.
  cfroot = root; 
}

CFTree::CFTree(CFNode* node) {
  fanout = 3; 
  maxentry = 10; 
  maxlevel = 5; 
  level = 1; 
  cfroot = node; 
}
CFTree::~CFTree() {}

// @brief Insert a transaction into entry. The insertion process
// will traverse the whole clustering tree till to the leaf entries.
// for each node, it tries to determine which child branch to go to. 
// @param trans the transaction to insert into. 
// @return entry id where the trans was inserted into.
int CFTree::insert_trans(map<string, int>& trans) {
  // start from the root node. 
  CFNode* maxn = cfroot; 	// best node to insert into. 
  CFTree* subtree; 
  while(1) {
    int eid = maxn->add_trans(trans);
    // if current node is leaf, insert the membership to members.
    if (maxn->isLeaf()) {
      // if node is overflow, split it and redistribute the entries. 
      if(maxn->isLeafOverflow(maxentry)) {
	split(maxn);
      } 
      return eid; 
    }
    // if not leaf node, insert to subtree recursively. 
    subtree = choose_subtree(trans, maxn->getChildren());
    subtree->insert_trans(trans);
  }  
}

// @brief split a leaf node. 
// @param en the node that are supposed to have the same parent as 
// the newly generated node. 
// @return true on success and false on failure. 
bool CFTree::split(CFNode* node) {
  if(!node->isLeaf()) {
    cerr << "split can only be initiated by leaf node. " << endl; 
    return false; 
  }

  // go up until to the root of tree to check if the parent node 
  // also needs split or not.
  CFNode *p = node; // tmp node to parent.
  while(true) {
    if (isOverFlow(p)) {
      if (p->isRoot()) {	// overflowed root. 
	CFNode* newroot = new CFNode(); 
	CFNode* newnode = new CFNode(); 
	
	// setup parent.
	newroot->setParent(NULL);
	p->setParent(newroot);
	newnode->setParent(newroot);
	
	// setup child nodes and reassign entries. 
	p->partition(newnode);
	newroot->addChild(p); 
	newroot->addChild(newnode);
      } else {			// overflowed nonroot. 
	CFNode* newnode = new CFNode(); 
	// parent and child relation. mutual. 
	newnode->setParent(p->getParent());
	p->getParent()->addChild(newnode);

	// partition entries evenly to two nodes. 
	p->partition(newnode); 
      }
    } else {
      // no overflow anymore, stop here. 
      break; 
    }
    p = p->getParent(); 
  }
  return true; 
}

// @brief test if a node is overflow or not. 
bool CFTree::isOverFlow(CFNode* node) {
  if (node->isLeaf()) {
    return node->isLeafOverflow(maxentry);
  } else {
    return node->isIndexOverflow(fanout); 
  }
}

// @brief adjust the membership of a transaction. 
// @param the transaction to be adjusted.
// @return the new entry id where the trans was adjusted to. 
int CFTree::adjust_trans(map<string, int>& trans, int eid) {
  // find the leaf node where this entry reside in. 
  CFNode* node = findEntry(eid); 

  // for all the entries in the leaf node, choose the one that
  // can achieve highest ewcd metric and adjust accordingly.
  float maxv = -1;
  float v;
  int  id; 
  map<int, Entry*>::iterator it = node->getEntries().begin(); 
  while(it++ != node->getEntries().end()) {
    if (eid == (it->second)->getEid()) {
      // the same entry, test removal. 
      v = (it->second)->test_trans(trans, 1);
      if (v > maxv) {
	maxv = v;
	id = eid; 
      }
    } else {
      // different entry. 
      v = (it->second)->test_trans(trans, 0);
      if (v > maxv) {
	maxv = v;
	id = (it->second)->getEid(); 
      }      
    }
  }
  // do adjustment. 
  node->getEntryById(eid)->remove_trans(trans); 
  node->getEntryById(id)->add_trans(trans); 
  return eid; 
}

// @brief Find where an entry with eid resides. 
CFNode* CFTree::findEntry(int eid) {
  CFNode* node = getRoot(); 
  while(1) {
    
  }
}

// @brief choose the subtree to achieve maximum EWCD. 
// if current node is leaf node, we compare the ewcd increment 
// over all the entries with creating a new entry. 
// @param trans the trans to test. 
// @param the children of current entry.
// @return the entry pointer where maximum EWCD is achived. 
CFTree* CFTree::choose_subtree(map<string, int>& trans, vector<CFNode*>& children) {
  CFNode *maxn;			// entry that can achive largest ewcd.
  float maxv = -1.0;		// maximum ewcd. 
  float v = 0.0; 		// temp

  vector<CFNode*>::iterator it = children.begin();
  while(it++ != children.end()) {
    if((*it)->isLeaf()) {
      v = (*it)->test_trans(trans);
      if(maxv < v) {
	maxv = v;
	maxn = *it;
      }
    }
  }
  // todo.
  return new CFTree(maxn);
}

// @brief traverse the tree and print out the summary info for each 
// entry. 
void CFTree::pprint() {
  cout << *this << endl; 
}

// @brief overloading operator<<. do a 
// preorder traversal of the tree. 
// @param out, the output stream. 
// @param cftree, the reference to the CFTree. 
// @return the output stream reference. 
ostream& operator<<(ostream& out, CFTree& cftree) {
  CFNode* node = cftree.getRoot(); 

  // visit current node first. 
  // if current node is leaf node, then output and return, 
  // else, print current node, and then visit all the child
  // nodes. 
  out << *node << endl; 
  if(node->isLeaf()) { return out; }
  else {
    vector<CFNode*>::iterator it = node->getChildren().begin(); 
    while(it++ != node->getChildren().end()) {
      CFTree* subtree = new CFTree(*it); 
      out << *subtree;
    } 
  }
}
