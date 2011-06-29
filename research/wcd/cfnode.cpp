#include "cfnode.h"
#include <assert.h>
#include <algorithm>

// @brief Constructor of CFNode class, by default, each node
// will contain at least one entry, if this node is a leaf node, 
// more entries can be added, if it is non-leaf node, this will 
// be the only entry in this node, and this entry will contain 
// the summary information of the subtree.
CFNode::CFNode() {
  level = 0; 
  entries.clear(); 
  incNumNodes(); 
  DBG_CFNODE("CFNode Created.");
}

// @brief Create new entry. 
Entry* CFNode::newEntry(){
  Entry* en = new (std::nothrow) Entry(); 
  if(en) {
    return en; 
  } else {
    cerr << "Can't create Entry object. " << endl; 
    return NULL; 
  }
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

// @brief partition the entries in *this* node and move
// raghly half of the content from *this* node to its'
// brother node.
// The partition should be based on the 
// @param brother - the brother node of *this* node. 
// @return true on success and false if error happens. 
bool CFNode::partition(CFNode* brother) {
  int cnt = 0; 
  int size = entries.size();

  map<int, Entry*>::iterator it;
  for(int i = 0; i < size/2; i++) {
    it = getEntries().begin(); 
    int maxwcd = -1;
    Entry *en;  
    int v;
    while(it != getEntries().end()) {
      v = it->second->getWcd(); 
      if(v > maxwcd) {
	maxwcd = v;
	en = it->second; 
      }
      it++; 
    }
    brother->addEntry(&en); 
    removeEntry(en);
  }
  return true; 
}

// @brief Insert a transaction into the node. 
// @param trans - the transaction to be added. 
// @param child - the child node of *this* node.
// @return the entry id where the trans was added into. 
int CFNode::insert_trans(map<string, int>& trans, CFNode** child) {
  //assert(this);
  // variables for operation. 
  CFNode *succ = NULL, *new_succ = NULL; 
  CFNode *brother; 
  Entry *summary; 
  int ret = NONE; // return value, 1 for split, 0 for not split. 
  //child = NULL;

  // insert entry into leaf node. 
  if(isLeaf() && !isOverflow()) {
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
    Entry* newen = newEntry();
    v = newen->test_trans(trans, ADD);
    if (v > maxv) {
      // add trans into new entry and insert entry into this node. 
      DBG_CFNODE("Adding trans into a new entry in node.");
      int entryid = newen->add_trans(trans);
      // add entry id to the global membership list. //TODO. 
      addEntry(&newen);
    } else {
      delete newen; 
      // add the trans into the existing trans.
      DBG_CFNODE("Adding trans into existing entry.");
      int entryid = maxe->add_trans(trans); 
      // TODO add to membership list. 
      addMembership(entryid); 
    }
    // if node is overflowed, split the node among current node 
    // and the brother node of this node. 
    if(isOverflow()) {
      // brother = (*my_tree)->newNode();
      CFNode *node = new CFNode();  
      *child = node; //new CFNode(); 
      //brother = my_tree->newNode(); 
      //brother->setLevel(getLevel());
      (*child)->setLevel(getLevel());
      //partition(brother);	// distribute entries in *node*.
      partition(*child);
      //child = &brother; 
      ret = SPLIT;
    } else { ret = NONE; } 
    return ret; 

  } else { // add entry into none-leaf node.

    DBG_CFNODE("Adding transaction into index node.");
    //entryid = 0;// TODO. //getEntryById(eid)->add_trans(trans);
    
    // choose the best child node, then add the trans summary 
    // into this the root of this subtree. 
    Entry* subtree;
    choose_subtree(trans, &subtree); 
    subtree->add_trans(trans); 

    // go to next level. 
    succ = subtree->get_child();
    if(succ) {
      ret = succ->insert_trans(trans, &new_succ);
    } else return NONE;

    // arrange index nodes.  // TODO, split has problem.
    if (SPLIT == ret) {
      summary = new Entry(); 
      new_succ->get_summary(&summary); 
      addEntry(&summary); 
      if (isOverflow()) {
	*child = new CFNode();
	//brother = my_tree->newNode(); 
	(*child)->setLevel(getLevel());
	//brother->setLevel(getLevel());
	partition(*child);
	//partition(brother); 
	//child = &brother; 
	ret = SPLIT; 
      } else { ret = NONE; }
    }
    return ret; 
  }
}

// @brief absort transaction into the constructed tree. 
// This is the second phase of the ewcd algorithm. 
// @param trans - transaction to be absorbed. 
// @param child - child node for recurssion. 
// @return bool - true / false => success/failure.
bool CFNode::absorb_trans(map<string, int>& trans, CFNode* node) {
  // find the best subtree. 
  bool ret = true; 
  DBG_CFNODE("Absorbing trans......"); 
  Entry* subtree;
  node->choose_subtree(trans, &subtree); 
  subtree->add_trans(trans); 

  // if *this* node is not leaf node, then go down the tree. 
  if (!node->isLeaf()) {
    CFNode *child = subtree->get_child(); 
    ret = absorb_trans(trans, child); 
  }
  return ret; 
}
// @brief choose the subtree to achieve maximum EWCD. 
// The standard is by comparing the ewcd incrent by testing 
// to add entry into new node. 
// @param trans the trans to test. 
// @return the entry pointer where maximum EWCD is achived. 
bool CFNode::choose_subtree(map<string, int>& trans, Entry **subtree) {
  float maxv = -1000.0;		// maximum ewcd. 
  float v = 0.0; 		// temp

  map<int, Entry*>::iterator it = getEntries().begin();
  while(it != getEntries().end()) {
    v = it->second->test_trans(trans, ADD);
    if(v > maxv) {
      maxv = v;
      *subtree = it->second;
    }
    it++;
  }
  return true;
}


// @brief Get aggregated summary of node. 
// @param none. 
// @return pointer to entry. 
bool CFNode::get_summary(Entry** summary) {
  map<int, Entry*>::iterator it = getEntries().begin(); 
  //*summary = newEntry();	// BUGGY!!! .
  try {
    while(it != getEntries().end()) {
      (**summary) += *(it->second);// use overloaded += operator.
      it++; 
    }
  }
  catch (...) {
    DBG_CATCH("Get summary error.");
    return false; 
  }
  return true; 
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
bool CFNode::addEntry(Entry** en) {
  // cout << "Adding entry to node." << endl; 
  if(!getEntryById((*en)->getEid())) {
    entries[(*en)->getEid()] = *en; 
    //cout << "========= " << entries[en->getEid()]->getSk2() << endl;
    DBG_CFNODE("Entry added to node.");
    return true; 
  } else {
    cerr << "Entry with id " << (*en)->getEid() 
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
