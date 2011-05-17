#include "ctree.h"

// @brief Cftree Constructor. 
// @param cap maxtrans in leaf node.
// @param fo fanout of nonleaf node. 
// @param level maximum level of tree structure. 
// @return None, constructor no return.
Cftree::Cftree(int cap, int fo, int level):capacity(cap) {
  maxtrans = cap;
  fanout = fo; 
  maxlevel = level; 
  // Initialize the tree with two entries on level 2. 
  
}

// @brief Insert a transaction into entry. 
// This function will iterator all the leaf nodes and find the one 
// that can maximize the weighted cluster density (EWCD). 
// And insert it into this entry. 
// @param trans the transaction to insert into. 
// @return entry id into which the transaction was inserted.
int Cftree::insert_trans(map<string, int>& trans, int eid) {
  // find the one to maximize eid. 
  Entry* maxc;
  if (eid == -1) {
    int maxeid = test_trans(0, trans, eid); 
    maxc = getEntryById(maxeid); 
  } else {
    maxc = getEntryById(eid); 
  }

  // check the capacity of the maxc entry.
  if (maxc.getNk() >= capacity) {
    // split of node comes here.
    // how do we distribute the contents in the current entry. 
    // 
    Entry *ent = maxc.split();
  }
  maxc->add_trans(trans);
  return maxc->getCid(); 
}

// @brief change a transaction from one cluster to another.
// @param the tranaction to be removed. 
// @param eid the id of entry to remove from. 
// @return true on successful adjustment and false if adjustment 
// was not done. 
bool Cftree::adjust_trans(map<string, int>& trans, int oldeid, int neweid) {
  if (oldeid == neweid) {
    return false;
  } 
  Entry* olden = cluster[oldeid];
  Entry* newen = cluster[neweid];
  // adjust now.
  if (olden && newen) {
    olden->remove_trans(trans);
    newen->add_trans(trans);
  } else {
    cerr << "Entry id error." << endl; 
  }
}

// @brief test and find the cluster to achieve maximum EWCD. 
// @param trans the trans to test. 
// @param type add(0) / remove(1)
// @param eid the entry id where this trans reside in. 
// @return the entry id where insertion of trans can achieve. 
int Cftree::test_trans(map<string, int>& trans, int type, int eid) {
  vector<Entry *>::iterator it = allentries.begin();
  Entry *maxc;			// entry that can achive largest ewcd.
  float maxv = -1.0;		// maximum ewcd. 
  float v = 0.0; 		// temp
  while(it != allentries.end()) {
    if((*it)->isLeaf()) {
      v = (*it)->test_trans(trans);
      if(maxv < v) {
	maxv = v;
	maxc = *it;
      }
    }
  }
  return maxc->getCid();
}
