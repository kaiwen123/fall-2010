#include "cfnode.h"
#include <algorithm>

// @brief Constructor. 
CFNode::CFNode() {
  Entry* entry = new Entry();
  entries[entry->getEid()] = entry;  
}

// @brief test for choosing the subtree. 
float CFNode::test_trans(map<string, int>& trans) {
  Entry* entry = getEntries().begin()->second; 
  return entry->test_trans(trans, 0);
}

// @brief get entry by entry id.
Entry* CFNode::getEntryById(int eid) {
  if(entries.find(eid) != entries.end()) {
    return entries[eid]; 
  } else return NULL; 
}

// @brief get sum wcd over all entries in current node. 
float CFNode::getSummaryWcd() {
  float wcd = 0.0; 
  map<int, Entry*>::iterator it = getEntries().begin(); 
  while(it++ != getEntries().end()) {
    wcd += it->second->getWcd(); 
  }
  return wcd; 
}

// @brief partition the children between *this* node
// and another node if node is non-leaf node. 
// And distribute ranking 
// The partition metric is based on
// the wcd metric. 
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
    sort(children.begin(), children.end()); 
    vector<CFNode*>::iterator it = children.begin(); 
    int childcnt = children.size(); 
    for(int i = 0; i < childcnt; i++, it++) {
      node->addChild(*it);
      removeChild(*it);
    }
    return true; 
  }
  // we can't partition between leaf and non-leaf nodes. 
  return false;
}

// @brief Add a transaction into the node. 
// @param trans - the transaction to be added. 
// @return the entry id where the trans was added into. 
int CFNode::add_trans(map<string, int>& trans) {
  // If this is a non-leaf node, then add to summary of this
  // node, else find the best entry that this entry can be 
  // added into and insert it into this entry. The second process 
  // involves checking the trans over all the entries in *this* 
  // node and find the one that maximized the ewcd. 
  
}

// @brief Add an entry to the node. 
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

// @brief remove a child from non-leaf node.
bool CFNode::removeChild(CFNode* child) {
  int childcnt = children.size(); 
  for(int i = 0; i < childcnt; i++) {
    if (children[i] == child) {
      children.erase(children.begin() + i);
      return true;
    }
  }
  return false; 
}

// operator <
bool CFNode::operator<(CFNode* node) {
  return (getSummaryWcd() < node->getSummaryWcd());
}

// operator<<.
ostream& operator<<(ostream& out, CFNode& node) {
  out << "level: " << node.level;
  map<int, Entry*>::iterator it = node.getEntries().begin(); 
  while(it++ != node.getEntries().end()) {
    out << *(it->second); 
  }
  return out;
}
