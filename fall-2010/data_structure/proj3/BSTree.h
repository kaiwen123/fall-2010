// -*- C++ -*-
/**
 * @file BSTree.h
 * @brief Definition of a BSTree and BTreeNode class.
 * BTreeNode class defines the structure of the node for BSTree. 
 * BSTree defines a binary search tree class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#ifndef _BSTreeClass_
#define _BSTreeClass_
#include <string>
#include <vector>
#include <iostream>
#include "Employee.h"

using namespace std; 

class BTreeNode {
 private:
  int eid_; 			/* The Employee ID. */
  BTreeNode *parent; 		/* parent node. */
  BTreeNode *left; 		/* left child. */
  BTreeNode *right;		/* right child. */
  Employee *employee;		/* employ related to this node. */

 public:
  // Constructors
  BTreeNode();
  BTreeNode(Employee *e); 

  // Destructor.
  ~BTreeNode();

  /* setters. */
  void setEid(int id) {eid_ = id;} 
  void setParent(BTreeNode *p) {parent = p;}
  void setLeftChild(BTreeNode *l) {left = l;}
  void setRightChild(BTreeNode *r) {right = r;}
  void setEmployeeRecord(Employee *e) {employee = e;}

  /* getters. */
  int getEid() const {return eid_;}
  BTreeNode* getParent() const {return parent;}
  BTreeNode* getLeftChild() const {return left;}
  BTreeNode* getRightChild() const {return right;}
  Employee* getEmployeeRecord() const {return employee;}

  /**
   * @brief Output BTreeNode info into output stream. 
   * 
   * @param out Output stream. 
   * @param n BTreeNode object to be output. 
   * @return Output stream.
   */
  friend ostream& operator<<(ostream& out, BTreeNode& n);
};

class BSTree {
 private:
  BTreeNode *root; 		/* root of tree. */
  int size; 			/* size of tree. */

 public:
  /**
   * @brief Default constructor. 
   */
  BSTree();
  /**
   * @brief Construct BSTree from a string list. 
   */
  BSTree(vector<string> &list);
  /**
   * @brief Construct BSTree from a BTreeNode list. 
   */
  BSTree(vector<BTreeNode*> &list);
  // Destructor.
  ~BSTree();

  /**
   * @brief getters. 
   */
  BTreeNode* getRoot() {return root;}

  // Construct tree, Insert, delete elements. 
  /**
   * @brief Build a BSTree from list. 
   * @param list The string list to build tree from.
   * @return true on success and false on failure. 
   */
  //bool buildTree(vector<BTreeNode> &list);

  /**
   * @brief Insert node into tree. 
   * @param eid the Employee Id to be inserted, a new BSTreeNode
   * object will be created implicitely. 
   * @param root root of BStree.
   * @param node node to be inserted. 
   * @return true on success and false on failure.
   */
  bool insertNode(BTreeNode *root, BTreeNode *node);

  /**
   * @brief Delete node from tree. 
   * @param key of node to be deleted. 
   * @return void.
   */
  void deleteNode(string key);

  /**
   * @brief Find node with smallest eid. 
   * @param none.
   * @return BTreeNode the element of this node should not be
   * altered. 
   */
  BTreeNode& findSmallest() const;

  /**
   * @brief Find node with largest eid. 
   * @param none.
   * @return BTreeNode the element of this node should not be
   * altered. 
   */
  BTreeNode& findLargest() const;

  // Tree traversers. 
  /**
   * @brief Pre-Order traversal of tree. 
   * @param none.
   * @return void.
   */
  void preOrderTraverse();

  /**
   * @brief In-Order traversal of tree. 
   * @param none.
   * @return void.
   */
  void inOrderTraverse();

  /**
   * @brief Post-Order traversal of tree. 
   * @param none.
   * @return void.
   */
  void postOrderTraverse();

  /**
   * @brief Output BSTree info into output stream. 
   * 
   * @param out Output stream. 
   * @param bstree BSTree object to be output. 
   * @return Output stream.
   */
  friend ostream& operator<<(ostream& out, BSTree& bstree); 
};
#endif	/* ifdef */
