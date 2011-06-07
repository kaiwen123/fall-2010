#include "cftree.h"
#include <queue>
#include <math.h>
// @brief CFTree Constructor and distructor.
// @param fo fanout of nonleaf node. 
// @param level maximum level of tree structure. 
// @return None, constructor no return.
CFTree::CFTree(int fo, int maxlvl) {
  maxentry = fo; 
  setUplimit(fo, maxlvl);
  cfroot = newNode(); // root node.
  cfroot->setLevel(0);
  cout << uplimit << endl;

  DBG_CFTREE("CFTree Initialized");
}

// @brief Destructor of tree. cleanup all nodes. 
CFTree::~CFTree() {
  vector<CFNode*>::iterator it = allnodes.begin(); 
  while(it != allnodes.end()) {
    if(*it) delete *it; 
    it++;
  }
}

// @brief Insert a transaction into entry. The insertion process
// will traverse the whole clustering tree till to the leaf entries.
// for each node, it tries to determine which child branch to go to. 
// In case when the leaf node overflows and needs to be splited, it
// will go back up from leaf until to root node to propagate the
// split. 
// @param trans the transaction to insert into. 
// @return entry id where the trans was inserted into.
void CFTree::insert_trans(map<string, int>& trans) {
  DBG_CFTREE("Inserting transaction into tree.");
  // start from the root node. 
  CFNode* rootptr = getRoot(); 
  CFNode* child; 

  // insert trans into tree. 
  int split_root = rootptr->insert_trans(trans, &child);

  // split root if need to. 
  if(SPLIT == split_root) {
    CFNode* newroot = newNode(); 
    newroot->setLevel(rootptr->getLevel() + 1);
    child->setLevel(rootptr->getLevel());
    cfroot = newroot; 

    // first partition the old root into to nodes. 
    rootptr->partition(child);
    
    // get summary from old root and new sibling 
    // and insert them into new root. 
    Entry* e1 = rootptr->get_summary(); 
    Entry* e2 = child->get_summary(); 
    e1->set_child(rootptr);
    e2->set_child(child); 
    newroot->addEntry(e1); 
    newroot->addEntry(e2);
  }
}

// @brief Setup total number of nodes in the tree according to
// fanout and level.
// @param none. 
// @return total number of nodes as int. 
int CFTree::setUplimit(int fanout, int maxlevel) {
  // calculate total number of nodes tree can host. 
  // this should be calculated from level of tree and 
  // fanout of index nodes and max entry in leaf nodes. 
  int limit = (pow(fanout, maxlevel+1) - 1) / (fanout - 1);
  DBG_CFTREE("Total number of nodes: " + itoa(limit));
  uplimit = limit; 
  return limit;
}

// @brief Create new node for tree. This function should
// be used when new nodes need to be created for tree. 
// And also, it will test if the tree is full. If the tree
// is already full, then new node creation will fail.
// @param none.
// @return Pointer to the new node. 
CFNode* CFTree::newNode() {
  if(isFull()) {
    cout << "Tree is full, new node creation failed." << endl;
    return NULL;
  }
  CFNode* cfnode = new CFNode(this, getMaxEntry()); 
  allnodes.push_back(cfnode); 
  return cfnode;
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
  while(it != node->getEntries().end()) {
    if (eid == (it->second)->getEid()) {
      // the same entry, test removal. 
      v = (it->second)->test_trans(trans, REMOVE);
      if (v > maxv) {
	maxv = v;
	id = eid; 
      }
    } else {
      // different entry. 
      v = (it->second)->test_trans(trans, ADD);
      if (v > maxv) {
	maxv = v;
	id = (it->second)->getEid(); 
      }      
    }
    it++;
  }
  // do adjustment. 
  node->getEntryById(eid)->remove_trans(trans); 
  node->getEntryById(id)->add_trans(trans); 
  return eid; 
}

// @brief Find where an entry with eid resides. The cftree 
// will be traversed in preorder. 
// @param eid the id of entry to be found. 
// @return pointer to the node that contains entry with
// provided entry eid. 
CFNode* CFTree::findEntry(int eid) {
  // search over all nodes.
  vector<CFNode*>::iterator it = allnodes.begin(); 
  while(it != allnodes.end()) {
    if ((*it)->containsEntry(eid)) {
      return *it; 
    }
    it++; 
  }
  cout << "Can't find entry with eid: " << eid << endl; 
  return NULL;
}

// @brief traverse the tree and print related info 
// for node and entries. And then, delete each node.
// @param none. 
// @return void. 
void CFTree::traverse(CFNode* node) {
  node->pprint(); 
  while(true) {
    map<int, Entry*>::iterator it = node->getEntries().begin(); 
    while(it != node->getEntries().end()) {
      CFNode* succ = it->second->get_child(); 
      traverse(succ); 
      it->second->del_child(); 
      it++; 
    }
    if(node->isLeaf()) return; 
  }
}

// @brief print out the result of tree. 
// @param none. 
// @return void. 
void CFTree::pprint() {
  queue<CFNode*> nodes; 
  nodes.push(getRoot());
  while (nodes.size()>0) {
    CFNode* node = nodes.front();
    map<int, Entry*>::iterator it = node->getEntries().begin(); 
    while(it != node->getEntries().end()) {
      cout << *(it->second); 
      CFNode* tnode = it->second->get_child(); 
      cout << "Entry: "<< it->first << " in node: " << node << endl; 
      it++;
      if (NULL != tnode) nodes.push(tnode); 
    }
    nodes.pop();
  }
}
