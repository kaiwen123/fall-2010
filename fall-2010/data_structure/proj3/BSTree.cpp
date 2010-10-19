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
BSTree::BSTree():root(NULL),size(0) {
}
 
// Construct a BSTree from a BTreeNode list. 
BSTree::BSTree(vector<BTreeNode*> &list) {

}

// Destructor.
BSTree::~BSTree() {}

// Insert a string into the BSTree. 
// A new BTreeNode object will be constructed implicitely. 
bool BSTree::insertNode(BTreeNode *root, BTreeNode *node) {
  cout << "Inserted Node with eid: " << node->getEid() << endl;
  if(!root) {			// No elements in the tree yet. 
    // Insert node into tree structure.
    node->setParent(root);
    node->setLeftChild(NULL); 
    node->setRightChild(NULL);
    size++;
    return true;
  } else {			// Tree no empty. 
    int eid = node->getEid();	// Eid of current node. 
    if(eid > root->getEid()) {	// insert into right subtree. 
      insertNode(root->getRightChild(), node);
    } else {			// insert into left subtree. 
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

// preorder traversal. 
void preOrderTraverse() {

}

// inorder traversal. 
void inOrderTraverse() {

}

// postorder traversal. 
void postOrderTraverse() {

}

// Overloading the << operator for the BSTree class. 
ostream& operator<<(ostream& out, BSTree& bstree) {

}
