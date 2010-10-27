// -*- C++ -*-
/**
 * @file BSTree.h
 * @brief Definition of a BSTree class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#include "BSTree.h"

// Constructor with Employee object pointer.
BTreeNode::BTreeNode(LinkedNode<Employee> *e):eid_(e->getValue().getEid()),
					      enode(e) {
  parent = left = right = NULL;
}
/////////////////////////////////////////////////////////////
// BSTree Functions. 
// pick a leaf node from the BSTree. 
BTreeNode* BSTree::pickLeafNode(BTreeNode* node) {
  // if node is null or this is the root of tree. 
  // simply return this node itself. 
  if(node == NULL || (node->getParent() == NULL) && node->isLeafNode()) {
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
  if(pnode->getLeftChild() == node) {	// left child.
    pnode->setLeftChild(NULL);
  } else if(pnode->getRightChild() == node) {
    pnode->setRightChild(NULL);
  }
  node->setParent(NULL);
  return node;
}

// Replace one node on tree with a given node.
bool BSTree::replaceNode(BTreeNode* snode, BTreeNode* dnode) {
  dnode->setParent(snode->getParent());
  dnode->setLeftChild(snode->getLeftChild());
  dnode->setRightChild(snode->getRightChild());
  return true;
}

// Insert a node into the tree.  
bool BSTree::insertNode(BTreeNode *root, BTreeNode *node) {
  BTreeNode *n = findNode(node->getEid());
  if(n) {
    cout << "This Eid " << n << " " 
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
  if(node->isLeafNode() && !node->getParent()) {
    delete node; 
    setNullRoot();
    size--;
    return true; 
  }
  if(!node) return false; 	// node doesn't exist. 
  // Delete a leaf node.
  if(node->isLeafNode()) {
    node = pickLeafNode(node); 
    delete node; size--;
    return true; 
  }
  // delete a node who has a direct successor.  
  BTreeNode *successor = getSmallest();
  successor = pickLeafNode(successor);
  replaceNode(node, successor);
  delete node; size--;
  return true;

  // finally, the third situation. 
  node->getLeftChild()->setParent(node->getParent());
  node->getParent()->setLeftChild(node->getLeftChild());
  delete node; size--;
  return true;
}

// delete a node directly. 
bool BSTree::deleteNode(BTreeNode *node) {
  if(!node) return false; 	// node is NULL.
  int eid = node->getEmployeeRecord()->getValue().getEid();
  return deleteNode(eid); 
}

// Delete node by eid.
bool BSTree::deleteByEid(LinkedSortedList<Employee>* employee, int
eid) {
  BTreeNode* node = findNode(eid);
  if(!node) {return false;}
#ifdef DEBUG_DELETE_BYEID
  cout << "Delete by eid " << node->getEid() << endl; 
#endif
  LinkedNode<Employee>* enode = node->getEmployeeRecord();
  deleteNode(node);
  employee->deleteNode(enode);
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
    cout << "Deleting node : " << root->getEid() << endl; 
    root = pickLeafNode(root);
    delete(root); size--;
  }
  setNullRoot();
  return true;
}

// Search node with Employee id. 
BTreeNode* BSTree::findNode(int eid) {
  BTreeNode *node = getRoot();
  while ((node != 0) && (node->getEid() != eid)) {
    if (node->getEid() > eid)
      node = node->getLeftChild();
    else
      node = node->getRightChild();
  }
#ifdef DEBUG_INDEX_SEARCH
  if(node) cout << node->getEid() << endl; 
#endif
  return node;
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
BTreeNode* BSTree::getSmallest() {
  BTreeNode *node = getRoot();
  while(node->getLeftChild()) {
    node = node->getLeftChild();
  }
  return node; 
}

// Find largest from tree. 
// For BStree, the right most node is our goal.
BTreeNode* BSTree::getLargest(){
  BTreeNode *node = getRoot();
  while(node->getRightChild()) {
    node = node->getRightChild();
  }
  return node;
}
