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
BTreeNode::BTreeNode(Employee *e):eid_(e->getEid()),parent(NULL),
				  left(NULL),right(NULL),
				  employee(e) {}

// BTreeNode functions. 
// Overloading BTreeNode << operator. 
ostream& operator<<(ostream& out, BTreeNode& n) {
}

// BSTree Functions. 
// Default Constructor.
BSTree::BSTree():root_(NULL),size(0) {
}
 
// Construct a BSTree from a BTreeNode list. 
BSTree::BSTree(vector<BTreeNode*> &list) {

}

// Destructor.
BSTree::~BSTree() {}

// Insert a string into the BSTree. 
// A new BTreeNode object will be constructed implicitely. 
bool BSTree::insertNode(BTreeNode *root, BTreeNode *node) {
  //cout << "Inserted Node with eid: " << node->getEid() << endl;
  if(!root_) {			// No elements in the tree yet. 
    // Insert node into tree structure.
    root_ = node; cout << "root is : " << root_ << endl;
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

// delete a node with given key. 
void BSTree::deleteNode(string key) {

}

// Find smallest element
BTreeNode& BSTree::findSmallest() const {

}

// Find largest from tree. 
BTreeNode& BSTree::findLargest() const {

}

// Search node with Employee id. 
bool BSTree::findNode(BTreeNode * root, int eid) {
  if(!root) return false; 	// Empty tree.
  if(root->getEid() == eid) return true; 
  findNode(root->getLeftChild(), eid);
  findNode(root->getRightChild(), eid);
  return false; 
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

}
