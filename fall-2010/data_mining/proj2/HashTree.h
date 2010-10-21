// -*- C -*-
/**
 * @file HashTree.h
 * @brief Definition of a HashTree class. 
 * The HashTree class is used to do the APIORI association rule
 * mining. The tree will be build according to the APRIORI
 * method. Generally, the HashTree class will provide the following
 * functions for the APRIORI method:
 * 1, Each level l represents itemsets with l item; 
 * 2, A hash function will be defined to hash the value to control the
 * height and width of the hash tree, usually, the hash function is a
 * mod of a number N, which will control the breadth of the tree. 
 * 3, One Hash Tree node can contain multiple itemsets, which are
 * stored in an array or list. 
 * 4, There is an index array within the hash tree which is used to
 * store the locations of node in each level. This can be done using
 * the linked list or the vector method (vector method will be better,
 * because we can directly use the STL vector structure.
 *
 * @author Shumin Guo (guo.18@wright.edu)
 * @timestamp Tue Oct 19 00:15:06 EDT 2010
 * @version 1.0.0
 */
// $Log$

#ifndef _HashTreeClass_
#define _HashTreeClass_ 
#include <vector>
#include "defs.h"
#include "Itemset.h"

using namespace std; 
class HashNode {
 private:    
  HashNode *parent;		/* parent of node. */
  vector<HashNode*> children; 	/* children of node. */
  vector<Itemset> freq_sets;	/* frequent itemsets. */

 public:
  /* getters. */
  HashNode* getParent() const {return parent;}
  vector<HashNode*> getChildren() const {return children;}
  vector<Itemset> getFreqsets() const {return freq_sets;}
};

class HashTree {
 private: 
  int level; 
  int num_nodes;
  HashNode *root; 
  //vector<> nodeindex; 		/* level node index of tree. */

 public:
  int getLevel() const {return level;}
  int getNumNodes() const {return num_nodes;}

};

#endif //ifdef
