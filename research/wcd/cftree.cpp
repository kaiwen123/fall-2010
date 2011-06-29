#include "cftree.h"
#include <queue>

// @brief CFTree Constructor and distructor.
// @return None, constructor no return.
CFTree::CFTree() {
  cfroot = new CFNode(); // root node.
  Entry* en = cfroot->newEntry();
  cfroot->addEntry(&en); 
  cfroot->setLevel(0);
  DBG_CFTREE("CFTree Initialized");
}

// @brief Destructor of tree. cleanup all nodes. 
CFTree::~CFTree() { }

// @brief Insert a transaction into entry. The insertion process
// will traverse the whole clustering tree till to the leaf entries.
// for each node, it tries to determine which child branch to go to. 
// In case when the leaf node overflows and needs to be splited, it
// will go back up from leaf until to root node to propagate the split. 
// @param trans the transaction to insert into. 
// @return entry id where the trans was inserted into.
void CFTree::insert_trans(map<string, int>& trans) {
  if (isFull()) {
    DBG_CFTREE("Tree is full, start absorption phase. "); 
    absort_trans(trans);
    return; 
  }
  DBG_CFTREE("Inserting transaction into tree.");
  // start from the root node. 
  CFNode* root = getRoot(); 
  CFNode* child; 

  // insert trans into tree. 
  int split_root = root->insert_trans(trans, &child);

  // split root if need to. 
  if(SPLIT == split_root) {
    CFNode* newroot = new CFNode(); 
    newroot->setLevel(root->getLevel() + 1);
    child->setLevel(root->getLevel());

    // get summary from old root and new sibling 
    // and insert them into new root. 
    Entry *e1 = new Entry(); 
    Entry *e2 = new Entry();
    root->get_summary(&e1); 
    child->get_summary(&e2); 
    e1->set_child(root);
    e2->set_child(child); 
    newroot->addEntry(&e1); 
    newroot->addEntry(&e2);

    cfroot = newroot; 
  }
  pprint();
  return;
}

// @brief Create new node for tree. This function should
// be used when new nodes need to be created for tree. 
// And also, it will test if the tree is full. If the tree
// is already full, then new node creation will fail.
// @param none.
// @return Pointer to the new node. 
// CFNode* CFTree::newNode() {
//   if(isFull()) {
//     cout << "Tree is full, new node creation failed." << endl;
//     return NULL;
//   }
//   CFNode* cfnode = new CFNode(); 
//   num_nodes++;
//   return cfnode;
// }

// @brief Absort transactions after the tree construction phase. 
// In this process, no new node will be created, and the trans 
// will be inserted in to the entry which can maximize ewcd. 
// @param trans - the transaction to be absorted. 
// @return bool - true on success and false on failure. 
bool CFTree::absort_trans(map<string, int>& trans) {
  return getRoot()->absorb_trans(trans, getRoot()); 
}

// @brief traverse the tree and print related info 
// for node and entries. And then, delete each node.
// @param none. 
// @return true (success) and false (failure). 
bool CFTree::traverse(CFNode* node) {
  if (!node) {
    cerr << "Traversal error, input node is null. " << endl;
    return false; 
  }
  node->pprint(); 
  while(true) {
    map<int, Entry*>::iterator it = node->getEntries().begin(); 
    while(it != node->getEntries().end()) {
      CFNode* succ = it->second->get_child(); 
      traverse(succ); 
      it->second->del_child(); 
      it++; 
    }
    if(node->isLeaf()) return true; 
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
      cout << it->second << " in " << node 
	   << " level: " << node->getLevel() 
	   << " child " << it->second->get_child() << endl; 
      it++;
      if (NULL != tnode) nodes.push(tnode); 
    }
    nodes.pop();
  }
}
