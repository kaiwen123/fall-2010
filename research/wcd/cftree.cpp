#include "cftree.h"
// ------------------------------------------------------------
// @file cftree.cpp Implementation of the CFTree structure. 
// @func constructor. 
// @func insert_trans - transaction insertion. 
// @func adjust_trans - adjustment of transactions.
// @func test_trans - find the best entry to insert trans into. 
// @func print, output etc. 
// ------------------------------------------------------------
// 
// @brief Cftree Constructor and distructor.
// @param cap maxtrans in leaf node.
// @param fo fanout of nonleaf node. 
// @param level maximum level of tree structure. 
// @return None, constructor no return.
Cftree::Cftree(int cap, int fo, int level) {
  maxtrans = cap;
  fanout = fo; 
  maxlevel = level; 
  // Initialize the tree with two entries on level 2.   
}

Cftree::~Cftree() {
  // destruct all entries in the tree; 
  map<int, Entry*>::iterator it = allentries.begin();
  while(it != allentries.end()) {
    cout << "Deleting entry " << it->first << endl;
    delete it->second;
    it++;
  }
}

// @brief Insert a transaction into entry. 
// @param trans the transaction to insert into. 
// @return entry id into which the transaction was inserted.
int Cftree::insert_trans(map<string, int>& trans) {
  // find the one to maximize eid. 
  Entry* maxc = getEntryById(test_trans(trans, 0)); 
  
  // check the num of trans of the maxc entry.
  if (maxc->getNk() >= maxtrans) {
    // split of node comes here.
    // how do we distribute the contents in the current entry. 
    // 
    Entry *ent = maxc->split();
  }
  maxc->add_trans(trans);
  return maxc->getEid(); 
}

// @brief split a leaf node. 
// @param en the node that are supposed to have the same parent as 
// the newly generated node. 
// @return true on success and false on failure. 
bool Cftree::split(Entry* en) {
  if (!en->isLeaf()) {
    cerr << "Only a leaf node can init split.." << endl; 
    return false; 
  }
  int eid = allentries.size() + 1;
  Entry *newen = new Entry(eid);
  newen->setLeaf(); 
  newen->setParent(en->getParent());
  
  // go up until to the root of tree to check if the parent node 
  // also needs split or not.
  Entry *p = en->getParent();		// tmp node to parent.
  int childcnt = en->getChildCount(); 
  while(true) {
    if (p->isOverFlow(p->getEid())) {
      if (p->isRoot()) {	// overflowed root. 
	Entry* newroot = new Entry(allentries.size() + 1);
	Entry* newentry = new Entry(allentries.size() + 1); 
	
	// setup parent and leaf property.
	newroot->setParent(NULL); newroot->unsetLeaf(); 
	newentry->setParent(newroot); newentry->unsetLeaf(); 
	
	// assign children of previous node to the new nodes. 
	newroot->insertChild(en); 
	newroot->insertChild(newentry);
	
	// partition half of children to new entry. 
	// currently this is done randomly. 
	// may need to consider the ewcd metric over all the entries,
	// so that we can partition according to similarity of
	// clusters.
	// TODO:
	map<int, Entry*>::iterator it = en->getChildren().begin();
	int cnt = 0; 
	while(1) {
	  newentry->insertChild(it->second); 
	  en->eraseChild(it->first); 
	  cnt++; it++;
	  if (cnt >= childcnt / 2) {
	    break; 
	  }
	}
      } else {			// overflowed nonroot. 
	Entry* newentry = new Entry(allentries.size() + 1);
	newentry->setParent(en->getParent());
	newentry->unsetLeaf();
	// partition entries evenly to two nodes. 
	map<int, Entry*>::iterator it = en->getChildren().begin();
	int cnt = 0; 
	while(1) {
	  newentry->insertChild(it->second);
	  en->eraseChild(it->first);
	  cnt++; it++;
	  if (cnt >= childcnt / 2) {
	    break; 
	  }
	}
      }
    } else {
      // no overflow anymore, stop here. 
      break; 
    }
    p = p->getParent(); 
  }
  return true; 
}

// @brief change a transaction from one entry to another.
// @param the tranaction to be removed. 
// @param eid the id of entry to remove from. 
// @return the new entry id where the trans was adjusted to. 
int Cftree::adjust_trans(map<string, int>& trans, int oldeid, int neweid) {
  if (oldeid == neweid) {
    return false;
  } 
  Entry* olden = allentries[oldeid];
  Entry* newen = allentries[neweid];
  // adjust now.
  if (olden && newen) {
    olden->remove_trans(trans);
    newen->add_trans(trans);
  } else {
    cerr << "Entry id error." << endl; 
  }
  return newen->getEid(); 
}

// @brief test and find the entry to achieve maximum EWCD. 
// @param trans the trans to test. 
// @param type add(0) / remove(1) / remove then add(2).
// @param eid the entry id where this trans reside in. 
// @return the entry id where insertion of trans can achieve. 
int Cftree::test_trans(map<string, int>& trans, int type, int member) {
  Entry *maxc;		// entry that can achive largest ewcd.
  float maxv = -1.0;		// maximum ewcd. 
  float v = 0.0; 		// temp

  map<int, Entry *>::iterator it = allentries.begin();
  if(0 == type) {			// addition. 
    while(it != allentries.end()) {
      if(((*it).second)->isLeaf()) {
	v = (it->second)->test_trans(trans, 0);
	if(maxv < v) {
	  maxv = v;
	  maxc = it->second;
	}
      }
      it++; 
    }
  } else if(1 == type) {			// removal and then addition. 
    if (member == -1) {
      cerr << "please provide the entry id to remove from! " << endl; 
      return -1; 
    }
    v = getEntryById(member)->test_trans(trans, 1); 
    // current entry is the best choice. 
    if (v > maxv) { 
      maxv = v; 
      maxc = getEntryById(member); 
    }
    // test all the other entries. 
    while(it != allentries.end()) {
      if(((it->second)->isLeaf()) && ((it->second)->getEid() != member)) {
	v = (it->second)->test_trans(trans, 0); 
	if(maxv < v) {
	  maxv = v; 
	  maxc = it->second; 
	}
      }
      it++; 
    }
  } else {
    cerr << "Wrong test_trans type, please choose from 0 and 1. " << endl; 
    return -1; 
  }
  return maxc->getEid();
}

// @brief get parent pointer of a node. 
// @param eid the entry id of the entry. 
// @return the parent entry of current entry with eid. 
// if entry does not exit, return NULL. 
Entry* Cftree::getNodeParent(int eid) {
  if (allentries.find(eid) != allentries.end()) {
    return allentries[eid]->getParent(); 
  } else {
    return NULL; 
  }
}

// @brief get children of a node. 
// @param the entry id of the entry to operate on. 
// @return reference to the children map of this entry. 
// in case if the entry doesn't exist, return NULL.
map<int, Entry*>& Cftree::getNodeChildren(int eid) {
  if (allentries.find(eid) != allentries.end()) {
    return allentries[eid]->getChildren();
  } else {
    cerr << "can't get entry with eid " << eid << endl; 
  }
}

// @brief get count of children for current node. 
// @param eid the entry id to operate on.
// @return number of children if entry exists. 
// -1 in case entry with eid doesn't exist.
int Cftree::getChildCount(int eid) {
  if (allentries.find(eid) != allentries.end()) {
    return allentries[eid]->getChildCount();
  } else {
    return -1; 
  }
}

// @brief If the nonleaf node already contains the maximum 
// number of child nodes. Or if the leaf node already contains 
// maximum number of transactions. 
// @param eid of the entry to operate on. 
// @return true if the node is already full. 
// false otherwise. 
bool Cftree::isOverFlow(int eid) {
  if (allentries.find(eid) == allentries.end()) {
    cerr << "Node with eid " << eid << " does not exist." << endl; 
    return false; 
  } 

  // different for leaf node and nonleaf node. 
  Entry* en = allentries[eid]; 
  if (en->isLeaf()) {		// leaf node. 
    return (en->getNk() >= maxtrans); 
  } else {			// nonleaf node. 
    return (en->getChildCount() >= fanout); 
  }
}
