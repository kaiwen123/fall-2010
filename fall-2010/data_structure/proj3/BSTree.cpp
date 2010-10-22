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
					      parent(NULL),
					      left(NULL),right(NULL),
					      enode(e) {}

// BTreeNode functions. 
// Overloading BTreeNode << operator. 
ostream& operator<<(ostream& out, BTreeNode& n) {
}

// Kill the content related to this node. 
// This function will be called by destructor. 
bool BTreeNode::killContent() {
  // LinkedNode<Employee>* e = getEmployeeRecord();
  // if(em) {
  //   elist->deleteNode(em);
  // }
}

// Test if *this* node is leaf node.
bool BTreeNode::isLeafNode() {
  return !(getRightChild() || getLeftChild());
}

// Test if *this* node is internal node.
bool BTreeNode::isInternalNode() {
  return (!getRightChild() || !getLeftChild());
}

// BSTree Functions. 
// Default Constructor.
BSTree::BSTree():root_(NULL),size(0) {
}

// pick a leaf node from the BSTree. 
BTreeNode* BSTree::pickLeafNode(BTreeNode* node) {
  // if node is null or this is the root of tree. 
  // simply return this node itself. 
  if(node == NULL || node->getParent() == NULL) {
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
  } else {
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


// Destructor.
BSTree::~BSTree() {}

// Insert a string into the BSTree. 
// A new BTreeNode object will be constructed implicitely. 
bool BSTree::insertNode(BTreeNode *root, BTreeNode *node) {
  //cout << "Inserted Node with eid: " << node->getEid() << endl;
  if(!root_) {			// No elements in the tree yet. 
    // Insert node into tree structure.
    root_ = node; // cout << "root is : " << root_ << endl;
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
  //cout << "Size of tree is : " << getSize() << endl;
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
bool BSTree::deleteNode(BTreeNode *root, int eid) {
  BTreeNode *node = findNode(root, eid);
  if(!node) return false; 	// node doesn't exist. 
  // Delete a leaf node.
  if(node->isLeafNode()) {
    node = pickLeafNode(node); 
    delete node;
    return true; 
  }
  // delete a node who has a direct successor.  
  BTreeNode *successor = findSmallest(node->getRightChild());
  successor = pickLeafNode(successor);
  replaceNode(node, successor);
  delete node; 
  return true;

  // finally, the third situation. 
  node->getLeftChild()->setParent(node->getParent());
  node->getParent()->setLeftChild(node->getLeftChild());
  delete node; 
  return true;
}

// Destroy the whole tree. 
// This function is generally used to destroy all the nodes of the
// tree, which usually include the employee records indexed by this
// tree.  
bool destroyTree(BTreeNode *root) {

}

// Find smallest element
BTreeNode* BSTree::findSmallest(BTreeNode *root) const {
  if(!root) {			// Empty tree.
    return NULL;
  }
  // If the left child of current node is null, 
  // return current node.
  if(root->getLeftChild() == NULL) {
    return root; 
  } else {
    findSmallest(root->getLeftChild());
  }
}

// Find largest from tree. 
BTreeNode* BSTree::findLargest(BTreeNode *root) const {
  if(!root) {			// Empty tree.
    return NULL;
  }
  // If the left child of current node is null, 
  // return current node.
  if(root->getRightChild() == NULL) {
    return root; 
  } else {
    findSmallest(root->getRightChild());
  }
}

// Search node with Employee id. 
BTreeNode* BSTree::findNode(BTreeNode * root, int eid) {
  if(!root) return NULL; 	// Empty tree.
  if(root->getEid() == eid) {
    root->getEmployeeRecord()->print(); 
    return root; 
  }
  findNode(root->getLeftChild(), eid);
  findNode(root->getRightChild(), eid);
  return NULL; 
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
