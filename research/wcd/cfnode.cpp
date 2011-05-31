#include "cfnode.h"
#include <algorithm>

// @brief Constructor of CFNode class, by default, each node
// will contain at least one entry, if this node is a leaf node, 
// more entries can be added, if it is non-leaf node, this will 
// be the only entry in this node, and this entry will contain 
// the summary information of the subtree.
// CFNode::CFNode() {
//   addEntry(new Entry());
//   DBG_CFNODE("CFNode created");
// }

CFNode::CFNode(CFTree* rt) {
  my_tree = rt; 
  DBG_CFNODE("CFNode Created.");
}

// @brief wcd test for choosing the subtree. 
// @param trans the transaction to test over. 
// @return the wcd change of adding trans to subtree root.
float CFNode::test_trans(map<string, int>& trans) {
  return getEntries().begin()->second->test_trans(trans, 0);
}

// @brief get entry by entry id.
// @param eid the entry id to get from this node. 
// @return pointer to the entry with eid or NULL if not exists.
Entry* CFNode::getEntryById(int eid) {
  if(entries.find(eid) != entries.end()) {
    return entries[eid]; 
  } else return NULL; 
}

// @brief partition the children between *this* node
// and another node if node is non-leaf node. And distribute
// ranking the partition metric is based on ewcd metric. 
bool CFNode::partition(CFNode* node) {
  // if the nodes are leaf, partition all entries in 
  // the node between current node and the param node.
  if (node->isLeaf() && isLeaf()) {
    // sort the entries according to the wcd.
    // sort(getEntries().begin(), getEntries().end());
    // give half of entries to node. 
    int entrycount = entries.size(); 
    map<int, Entry*>::iterator it = entries.begin(); 
    for(int i = 0; i < entrycount / 2; i++, it++) {
      node->addEntry(it->second); 
      removeEntry(it->second);
    }
    return true; 
  }
  if ((!node->isLeaf()) && (!isLeaf())) {
    // TODO....
    //    sort(children.begin(), children.end()); 
    // vector<CFNode*>::iterator it = children.begin(); 
    // int childcnt = children.size(); 
    // for(int i = 0; i < childcnt; i++, it++) {
    //   //node->addChild(*it);
    //   removeChild(*it);
    // }
    return true; 
  }
  // we can't partition between leaf and non-leaf nodes. 
  return false;
}

// @brief Add a transaction into the node. 
// @param trans - the transaction to be added. 
// @return the entry id where the trans was added into. 
int CFNode::add_trans(map<string, int>& trans) {
  int entryid = 0; 
  if(isLeaf()) {
    // If this node is leaf node, then find the best entry that this
    // trans can be added into and insert it into this entry. 
    // This process involves checking the trans over all the entries
    // in *this* node and find the one that maximized the ewcd. 
    DBG_CFNODE("Adding transaction into leaf node.");
    float v, maxv = -1; 
    Entry* maxe = NULL; 	// entry that can maximize ewcd. 
    map<int, Entry*>::iterator it = getEntries().begin(); 
    while(it != getEntries().end()) {
      v = it->second->test_trans(trans, 0);
      if(v > maxv) {
	maxv = v; 
	maxe = it->second; 
      }
      it++;
    }
    // test the ewcd by creating a new entry. 
    Entry* newen = new Entry();
    v = newen->test_trans(trans, 0);
    if (v > maxv) {
      // add trans into new entry and insert entry into this node. 
      DBG_CFNODE("Adding trans into a new entry in node.");
      entryid = newen->add_trans(trans);
      addEntry(newen);
    } else {
      delete newen; 
      // add the trans into the existing trans.
      DBG_CFNODE("Adding trans into existing entry.");
      entryid = maxe->add_trans(trans); 
    }
  } else {
    // If this is a non-leaf node, then add to summary of this node.
    DBG_CFNODE("Adding transaction into index node.");
    entryid = 0;// TODO. //getEntryById(eid)->add_trans(trans);
  }
  return entryid; 
}

// @brief Get aggregated summary of node. 
// @param none. 
// @return pointer to entry. 
Entry* CFNode::get_summary() {
  map<int, Entry*>::iterator it = getEntries().begin(); 
  Entry* en = new Entry(my_tree);
  while(it != getEntries().end()) {
    (*en) += *(it->second);// use overloaded += operator.
    it++; 
  }
  return en; 
}

// @brief Get total transactions in this node. 
// @param none. 
// @return total number of transactions in *this* node.
int CFNode::get_num_trans() {
  int trans_cnt = 0; 
  map<int, Entry*>::iterator it = getEntries().begin(); 
  while(it != getEntries().end()) {
    trans_cnt += it->second->getNk(); 
    it++;
  }
  return trans_cnt;
}

// @brief Find entry by pointer
// @brief Add an entry to the node. 
// @param en entry pointer to add onto. 
// @return true on success and false on failure. 
bool CFNode::addEntry(Entry* en) {
  if(!getEntryById(en->getEid())) {
    entries[en->getEid()] = en; 
    return true; 
  } else {
    cerr << "Entry with id " << en->getEid() 
	 << " already exists. " << endl;
    return false; 
  }
}

// @brief remove an entry from node.
// @param en entry pointer to remove. 
// @return true on success and false on failure. 
bool CFNode::removeEntry(Entry* en) {
  if(!getEntryById(en->getEid())) {
    cerr << "Entry with id " << en->getEid() 
	 << " doesn't exists. " << endl;
    return false; 
  } else {
    entries.erase(en->getEid());
    return true; 
  } 
}

// @brief operator<<.
// @param out output stream. 
// @param node the node for output. 
// @return output stream.
ostream& operator<<(ostream& out, CFNode& node) {
  out << "Node:" << &node << endl;
  map<int, Entry*>::iterator it = node.getEntries().begin(); 
  while(it != node.getEntries().end()) {
    out << *(it->second); 
    it++;
  }
  return out;
}
// @brief print the entries in this node. 
// @param none. 
// @return void. 
void CFNode::pprint() {
  map<int, Entry*>::iterator it = getEntries().begin();
  while (it != getEntries().end()) {
    cout << *(it->second) << endl;
    it++;
  }
  return;
}
