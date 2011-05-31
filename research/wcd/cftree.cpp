#include "cftree.h"
#include <queue>
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
  level = 1;

  // Initialize the tree with two entries on level 2.   
  cfroot = new CFNode(this); 

  DBG_CFTREE("CFTree Initialized");
}

CFTree::~CFTree() {}

// @brief Insert a transaction into entry. The insertion process
// will traverse the whole clustering tree till to the leaf entries.
// for each node, it tries to determine which child branch to go to. 
// @param trans the transaction to insert into. 
// @return entry id where the trans was inserted into.
int CFTree::insert_trans(map<string, int>& trans) {
  DBG_CFTREE("Inserting transaction into tree.");
  // start from the root node. 
  CFNode* node = getRoot(); 	// best node to insert into. 
  while(1) {
    int eid = node->add_trans(trans);
    // if current node is leaf, insert the membership to members.
    if (node->isLeaf()) {
      // if node is overflow, split it and redistribute the entries. 
      if(node->isOverflow()) {
	DBG_CFTREE("Splitting overflowed leaf node.");
	split(node);
      } 
      return eid; 
    }
    // if not leaf node, insert to subtree recursively. 
    DBG_CFTREE("Recursively insert trans into subtrees.");
    //node = choose_subtree(trans, node->getChildren());
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
  while(true) // {
  //   if (isOverFlow(p)) {
  //     if (p->isRoot()) {	// overflowed root. 
  // 	DBG_CFNODE_SPLIT("Splitting leaf node.");
  // 	CFNode* newroot = new CFNode(); 
  // 	CFNode* newnode = new CFNode(); 
	
  // 	// setup parent.
  // 	newroot->setParent(NULL);
  // 	p->setParent(newroot);
  // 	newnode->setParent(newroot);

  // 	// assign new root.
  // 	cfroot = newroot; 
	
  // 	// setup child nodes and reassign entries. 
  // 	p->partition(newnode);
  // 	newroot->addChild(p); 
  // 	newroot->addChild(newnode);
  // 	DBG_CFNODE_SPLIT("Split done at root.");
  // 	break;			// split is done at root. 
  //     } else {			// overflowed nonroot. 
  // 	DBG_CFNODE_SPLIT("Splitting non-leaf node.");
  // 	CFNode* newnode = new CFNode(); 
  // 	// parent and child relation. mutual. 
  // 	newnode->setParent(p->getParent());
  // 	p->getParent()->addChild(newnode);

  // 	// partition entries evenly to two nodes. 
  // 	//p->partition(newnode); 
  //     }
  //   } else {
  //     // no overflow anymore, stop here. 
  //     DBG_CFNODE_SPLIT("Split done.");
  //     break; 
  //   }
  //   p = p->getParent(); 
  // }
  return true; 
}

// @brief Setup total number of nodes in the tree according to
// fanout and level.
// @param none. 
// @return total number of nodes as int. 
int CFTree::setUplimit() {
  // calculate total number of nodes tree can host. 
  // this should be calculated from level of tree and 
  // fanout of index nodes and max entry in leaf nodes. 
  // TODO
  return fanout * maxentry; 
}

// @brief Create new node for tree. This function should
// be used when new nodes need to be created for tree. 
// @param none.
// @return Pointer to the new node. 
CFNode* CFTree::newNode() {
  CFNode* cfnode = new CFNode(this); 
  allnodes.push_back(cfnode); 
  return cfnode;
}

// @brief adjust the membership of a transaction. 
// @param the transaction to be adjusted.
// @return the new entry id where the trans was adjusted to. 
int CFTree::adjust_trans(map<string, int>& trans, int eid) {
  // find the leaf node where this entry reside in. 
  CFNode* node = findEntry(getRoot(), eid); 

  // for all the entries in the leaf node, choose the one that
  // can achieve highest ewcd metric and adjust accordingly.
  float maxv = -1;
  float v;
  int  id; 
  map<int, Entry*>::iterator it = node->getEntries().begin(); 
  while(it != node->getEntries().end()) {
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
CFNode* CFTree::findEntry(CFNode* node, int eid) {
  if(node->containsEntry(eid)) {
    return node; 
  }
  // search over all children nodes.
  // TODO. 
  // vector<CFNode*>::iterator it = node->getChildren().begin(); 
  // while(it != node->getChildren().end()) {
  //   findEntry((*it), eid); 
  //   it++; 
  // }
  return NULL;
}

// @brief choose the subtree to achieve maximum EWCD. 
// if current node is leaf node, we compare the ewcd increment 
// over all the entries with creating a new entry. 
// @param trans the trans to test. 
// @param the children of current entry.
// @return the entry pointer where maximum EWCD is achived. 
CFNode* CFTree::choose_subtree(map<string, int>& trans, vector<CFNode*>& children) {
  CFNode *maxn;			// entry that can achive largest ewcd.
  float maxv = -1.0;		// maximum ewcd. 
  float v = 0.0; 		// temp

  vector<CFNode*>::iterator it = children.begin();
  while(it != children.end()) {
    if((*it)->isLeaf()) {
      v = (*it)->test_trans(trans);
      if(maxv < v) {
	maxv = v;
	maxn = *it;
      }
    }
    it++;
  }
  // todo.
  return maxn;
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
  // nodes.push(getRoot());
  // while (nodes.size()>0) {
  //   CFNode* node = nodes.front();
  //   cout << *node << endl; 
  //   vector<CFNode*>::iterator it = node->getChildren().begin(); 
  //   while(it != node->getChildren().end()) {
  //     nodes.push(*it);
  //     it++;
  //   }
  //   nodes.pop();
  // }
}

// @brief overloading operator<<. do a 
// preorder traversal of the tree. 
// @param out, the output stream. 
// @param cftree, the reference to the CFTree. 
// @return the output stream reference. 
ostream& operator<<(ostream& out, CFNode* node) {
  // preorder traversal of the tree. 
  out << *node << endl; 
  // if(node->isLeaf()) { return out; }
  // vector<CFNode*>::iterator it = node->getChildren().begin(); 
  // while(it != node->getChildren().end()) {
  //   out << *it;
  //   it++;
  // }
}
