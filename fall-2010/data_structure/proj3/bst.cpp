// -*- C++ -*-
/**
 * @file bst.h
 * @brief Definition of a BTreeNode and BSTree classes. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#include "bst.h"

// Constructor with Employee object pointer.
BTreeNode::BTreeNode(LinkedNode<Employee> *e):eid_(e->getValue().getEid()),
					      enode(e) {
  parent = left = right = NULL;
}
////////////////////////////////////////////////////////////
// BSTree Functions. 
// pick a leaf node from the BSTree. 
BTreeNode* BSTree::pickLeafNode(BTreeNode* node) {
  // if node is null or this is the root of tree. 
  // return this node. 
  if(node == NULL || ((node->getParent() == NULL) && node->isLeafNode())) {
    return node; 
  }
  if(!node->isLeafNode()) {
    cout << "Sorry, this function only work for leaf node. " 
	 << "You can test node with isLeafNode() "
	 << "before using this function. " << endl; 
    return NULL;
  }
  // Now, let's pick...
  BTreeNode *pnode = node->getParent(); // parent.
  // remove linkage with the parent node. 
  if(pnode->getLeftChild() == node) {	// left child.
    pnode->setLeftChild(NULL);
  } else {
    pnode->setRightChild(NULL);
  }
  node->setParent(NULL);
  return node;
}

// Replace one node on tree with a given node.
bool BSTree::replaceNode(BTreeNode* snode, BTreeNode* dnode) {
  if(snode->isRoot()) setRoot(dnode); // set root. 
  // dnode is from right subtree.
  if(dnode->getEid() > snode->getEid()) { 
    // Let the destination node release its resources. 
    // The dnode has a right child, it is guaranteed to have no left
    // children. 
    if(dnode->getRightChild()){
      if((dnode->getParent()) && (dnode->getParent() != snode)) {
	dnode->getParent()->setLeftChild(dnode->getRightChild());
      }
    }
    // Setting up new connection with other nodes. 
    // Setting children.
    dnode->setLeftChild(snode->getLeftChild());
    // Avoid replacing by itself. 
    if(snode->getRightChild() != dnode) {
      dnode->setRightChild(snode->getRightChild());
    }
    // Setting parent. 
    if(snode->getParent()) {
      if(snode->getParent()->getRightChild() == snode) {
	snode->getParent()->setRightChild(dnode);
      } else {
	snode->getParent()->setLeftChild(dnode);
      }
    } else {
      setRoot(dnode);
    }
  } else {			// dnode is from left subtree. 
    // release connection for dnode. 
    if(dnode->getLeftChild()){
      if((dnode->getParent()) && (dnode->getParent() != snode)) {
	dnode->getParent()->setRightChild(dnode->getLeftChild());
      }
    }
    // settting up the children.
    dnode->setRightChild(snode->getRightChild());
    // Avoid replacing by itself. 
    if(snode->getLeftChild() != dnode) {
      dnode->setLeftChild(snode->getLeftChild());
    }
    // Setting up the parent. 
    if(snode->getParent()) {
      if(snode->getParent()->getRightChild() == snode) {
	snode->getParent()->setRightChild(dnode);
      } else {
	snode->getParent()->setLeftChild(dnode);
      }
    } else {
      setRoot(dnode);
    }
  }
  return true;
}

// Insert a node into the tree.  
bool BSTree::insertNode(BTreeNode *root, BTreeNode *node) {
  BTreeNode *n = findNode(node->getEid());
  if(n) {
    cout << "This Eid " << " " 
	 << node->getEmployeeRecord()->getValue().getEid() 
	 << " already exists in the index. " << endl;  
    return false;
  }
  if(!root_) {			// No elements in the tree yet. 
    // Insert node into tree structure.
    root_ = node;
    size++;
    return true;
  }
  // Tree not empty. 
  int eid = node->getEid();	// Eid of current node. 
  if(!root) {
    root = root_;		// only root node is there.
  }
  if(eid > root->getEid()) {	// insert into right subtree. 
    if(!root->getRightChild()) { // right child is null - insert.
      root->setRightChild(node);
      size++;
    } else {			// right child is not null - recursive.
      insertNode(root->getRightChild(), node);
    }
  } else {			// insert into left subtree. 
    if(!root->getLeftChild()) { // right child is null - insert.
      root->setLeftChild(node);
      size++;
    } else {			// right child is not null - recursive.
      insertNode(root->getLeftChild(), node);
    }
  }
  return true;
}

// delete a node with given eid. 
// There are three possible situations for this action. 
// 1, node is a leaf node, pick this node from tree then delete it. 
// 2, node has direct successor, replace it with its successor and
//    then delete the node. 
// 3, neither of above, node has a predecessor, work the similar way
//    as deleting a node from a linked list. Nodes are linked by left
//    child and parent pointers.
bool BSTree::deleteNode(int eid) {
  BTreeNode *node = findNode(eid);
  if(!node) return false; 	// node doesn't exist. 
  if(node->isLeafNode() && (!node->getParent())) { // root.
#ifdef DEBUG_DELETE_BYEID
    cout << "root node deleted." << endl; 
#endif
    delete node; setNullRoot(); size--;
    return true; 
  }
  if(node->isLeafNode()) {	  // leaf node. 
#ifdef DEBUG_DELETE_BYEID
    cout << "leaf node deleted." << endl;
    cout << "The parent of this node is: " 
	 << node->getParent()->getEid() << endl;
#endif
    node = pickLeafNode(node); 
    delete node; size--;
    return true; 
  }
  // delete a node with a direct successor.
  if(node->getRightChild()){
    BTreeNode *successor = getSmallest(node->getRightChild());
    if(successor->isLeafNode()) successor = pickLeafNode(successor);
#ifdef DEBUG_DELETE_BYEID
    cout << "Before Replace: " 
	 << "Current: " << node->getEid() 
	 << " Replace by successor: " 
	 << successor->getEid() << endl; 
#endif
    replaceNode(node, successor);
    // successor->setLeftChild(node->getLeftChild());
    // if(node->getParent()) {
    //   if(node->getParent()->getRightChild() == node) {
    // 	node->getParent()->setRightChild(successor);
    //   } else {
    // 	node->getParent()->setLeftChild(successor);
    //   }
    // } else {
    //   setRoot(successor);
    // }

#ifdef DEBUG_DELETE_BYEID
    cout << "After Replace: " 
	 << "Current: " << node->getEid() 
	 << " Replace by successor: " 
	 << successor->getEid() << endl; 
#endif
    delete node; size--;
    return true;
  }
  // finally, the third situation. 
  if(node->getLeftChild()) {
    replaceNode(node, node->getLeftChild());
    // node->getLeftChild()->setRightChild(node->getRightChild());
    // if(node->isRoot()) setRoot(node->getLeftChild());
    // else node->getParent()->setLeftChild(node->getLeftChild());
#ifdef DEBUG_DELETE_BYEID
    cout << "Replace by predecessor." << endl; 
#endif
    delete node; size--;
    return true;
  }
  return false; 
}

// delete a node directly. 
bool BSTree::deleteNode(BTreeNode *node) {
  if(!node) return false; 	// node is NULL.
  int eid = node->getEmployeeRecord()->getValue().getEid();
  return deleteNode(eid); 
}

// Delete node by eid.
bool BSTree::deleteByEid(LinkedSortedList<Employee>* employee, 
			 int eid) {
  BTreeNode* node = findNode(eid);
  if(!node) {
    cout << "Employee Id " << eid << " doesn't exist. " << endl; 
    return false;
  }
#ifdef DEBUG_DELETE_BYEID
  cout << "Before Deleting: " << node->getEid() << endl; 
  cout << "In Order traverse: " << endl; 
  inOrderTraverse(getRoot());
  cout << "Pre Order traverse: " << endl; 
  preOrderTraverse(getRoot());
#endif
  LinkedNode<Employee>* enode = node->getEmployeeRecord();
  deleteNode(eid);
  employee->deleteNode(enode);
#ifdef DEBUG_DELETE_BYEID
  cout << "After Deleting: " << node->getEid() << endl; 
  cout << "In Order traverse: " << endl; 
  inOrderTraverse(getRoot());
  cout << "Pre Order traverse: " << endl; 
  preOrderTraverse(getRoot());
#endif
  return true; 
}

// Destroy the whole tree. 
// This function is generally used to destroy all the nodes of the
// tree, which usually include the employee records indexed by this
// tree.  
bool BSTree::destroyTree(BTreeNode* root) {
  if(root != NULL) {
    destroyTree(root->getLeftChild());
    destroyTree(root->getRightChild());
#ifdef DEBUG_DELETE_TREE
    cout << "Deleting node : " << root->getEid() << endl; 
#endif
    root = pickLeafNode(root);
    delete(root); size--;
  }
  setNullRoot();
  return true;
}

// Search node with Employee id. 
BTreeNode* BSTree::findNode(int eid) {
  BTreeNode *node = getRoot();
  while ((node != NULL) && (node->getEid() != eid)) {
    if (node->getEid() > eid) {
      node = node->getLeftChild();
    } else {
      node = node->getRightChild();
    }
  }
#ifdef DEBUG_INDEX_SEARCH
  if(node) cout << "Found Node with EID: " << node->getEid() << endl; 
  cout << "Current size of tree. " << getSize() << endl;
#endif
  return node;
}

void BSTree::findByEid(int eid) {
  int cnt = 0;		  // count how many index nodes are searched. 
  BTreeNode *node = getRoot();
  while ((node != 0) && (node->getEid() != eid)) {
    if (node->getEid() > eid) {
      node = node->getLeftChild();
      cnt++;
    } else {
      node = node->getRightChild();
      cnt++;
    }
  }
  cnt++;
  cout << "\n" << cnt << " index nodes searched. Found";
  if(node) {
    cout << " 1 record:" << endl << endl; 
    node->getEmployeeRecord()->getValue().print();
  } else {
    cout << " 0 record:" << endl << endl;
  }
}

// preorder traversal. 
void BSTree::preOrderTraverse(BTreeNode *root) {
  if(!root) return;		// empty tree. 
  root->visit();		// visit root node. 
  preOrderTraverse(root->getLeftChild()); // visit left child.
  preOrderTraverse(root->getRightChild()); // visit right child.
}

// inorder traversal. 
void BSTree::inOrderTraverse(BTreeNode *root) {
  if(!root) return;		// empty tree. 
  inOrderTraverse(root->getLeftChild()); // visit left child.
  root->visit();		// visit root node. 
  inOrderTraverse(root->getRightChild()); // visit right child.
}

// postorder traversal. 
void BSTree::postOrderTraverse(BTreeNode *root) {
  if(!root) return;		// empty tree. 
  postOrderTraverse(root->getLeftChild()); // visit left child.
  postOrderTraverse(root->getRightChild()); // visit right child.
  root->visit();		// visit root node. 
}

// Overloading the << operator for the BSTree class. 
ostream& operator<<(ostream& out, BSTree& bstree) {
  bstree.inOrderTraverse(bstree.getRoot());
  out << "";			// return nothing to out stream. 
  return out;
}

// Find smallest element
// For BStree, the left most node is our goal.
BTreeNode* BSTree::getSmallest(BTreeNode *root) {
  while(root->getLeftChild()) {
    root = root->getLeftChild();
  }
  return root; 
}

// Find largest from tree. 
// For BStree, the right most node is our goal.
BTreeNode* BSTree::getLargest(BTreeNode *root){
  while(root->getRightChild()) {
    root = root->getRightChild();
  }
  return root;
}
