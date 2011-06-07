#include "cfnode.h"
#include <algorithm>

// @brief Constructor of CFNode class, by default, each node
// will contain at least one entry, if this node is a leaf node, 
// more entries can be added, if it is non-leaf node, this will 
// be the only entry in this node, and this entry will contain 
// the summary information of the subtree.
CFNode::CFNode(CFTree* rt, int cap) {
  capacity = cap; 
  my_tree = rt; 
  DBG_CFNODE("CFNode Created.");
}

// @brief Create new entry. 
Entry* CFNode::newEntry(){
  return (new Entry()); 
}

// @brief destructor for CFNode class, mainly used to delete 
// all entries in node.
CFNode::~CFNode() {
  map<int, Entry*>::iterator it = getEntries().begin(); 
  while( it != getEntries().end()) {
    delete it->second;
    it++;
  }
}

// @brief wcd test for choosing the subtree. 
// @param trans the transaction to test over. 
// @return the wcd change of adding trans to subtree root.
float CFNode::test_trans(map<string, int>& trans) {
  return getEntries().begin()->second->test_trans(trans, ADD);
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

// @brief Insert a transaction into the node. 
// @param trans - the transaction to be added. 
// @param child - the child node of *this* node.
// @return the entry id where the trans was added into. 
int CFNode::insert_trans(map<string, int>& trans, CFNode** child) {
  // variables for operation. 
  CFNode *succ, *new_succ; 
  CFNode *brother; 
  Entry *de, *summary; 
  int ret; // return value, 1 for split, 0 for not split. 

  if(isLeaf() && !my_tree->isFull()) {
    // If this node is leaf node, then find the best entry that this
    // trans can be added into and insert it into this entry. 
    // This process involves checking the trans over all the entries
    // in *this* node and find the one that maximized the ewcd. 
    DBG_CFNODE("Adding transaction into tree node.");
    float v, maxv = -1; 
    Entry* maxe = NULL; 	// entry that can maximize ewcd. 
    map<int, Entry*>::iterator it = getEntries().begin(); 
    while(it != getEntries().end()) {
      v = it->second->test_trans(trans, ADD);
      if(v > maxv) {
	maxv = v; 
	maxe = it->second; 
      }
      it++;
    }
    // test the ewcd by creating a new entry. 
    Entry* newen;
    v = newen->test_trans(trans, ADD);
    if (v > maxv) {
      // add trans into new entry and insert entry into this node. 
      DBG_CFNODE("Adding trans into a new entry in node.");
      newen = newEntry(); 
      int entryid = newen->add_trans(trans);
      // add entry id to the global membership list. //TODO. 
      addEntry(newen);
    } else {
      // add the trans into the existing trans.
      DBG_CFNODE("Adding trans into existing entry.");
      int entryid = maxe->add_trans(trans); 
      // TODO add to membership list. 
      addMembership(entryid); 
    }
    // if node is overflowed, split the node among current node 
    // and the brother node of this node. 
    if(isOverflow()) {
      brother = my_tree->newNode(); 
      brother->setLevel(getLevel());
      partition(brother);	// distribute entries in *node*.
      *child = brother; 
      ret = SPLIT;
    } else {
      ret = NONE; 
    }
    return ret; 

  } else { // none-leaf node.

    DBG_CFNODE("Adding transaction into index node.");
    //entryid = 0;// TODO. //getEntryById(eid)->add_trans(trans);
    
    // choose the best child node, then add the trans summary 
    // into this the root of this subtree. 
    Entry* e = choose_subtree(trans); 
    e->add_trans(trans); 

    // go to next level. 
    succ = e->get_child();
    ret = succ->insert_trans(trans, &new_succ);
    return ret; 

    // arrange index nodes.  
    if (SPLIT == ret) {
      de = new_succ->get_summary(); 
      addEntry(de); 
      if (isOverflow()) {
	brother = my_tree->newNode(); 
	brother->setLevel(getLevel());
	partition(brother); 
	*child = brother; 
	ret = SPLIT; 
      } else {
	ret = NONE; 
      }
      return ret; 
    } else {
      return NONE; 
    }
  }
}

// @brief choose the subtree to achieve maximum EWCD. 
// The standard is by comparing the ewcd incrent by testing 
// to add entry into new node. 
// @param trans the trans to test. 
// @return the entry pointer where maximum EWCD is achived. 
Entry* CFNode::choose_subtree(map<string, int>& trans) {
  Entry *maxe;			// entry that can achive largest ewcd.
  float maxv = -1.0;		// maximum ewcd. 
  float v = 0.0; 		// temp

  map<int, Entry*>::iterator it = getEntries().begin();
  while(it != getEntries().end()) {
    v = it->second->test_trans(trans, ADD);
    if(maxv < v) {
      maxv = v;
      maxe = it->second;
    }
    it++;
  }
  return maxe;
}


// @brief Get aggregated summary of node. 
// @param none. 
// @return pointer to entry. 
Entry* CFNode::get_summary() {
  map<int, Entry*>::iterator it = getEntries().begin(); 
  Entry* en = newEntry();
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
