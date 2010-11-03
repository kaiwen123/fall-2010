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

#include "defs.h"
#include "Itemset.h"

using namespace std; 

class HashNode {
 private:
  int level;		    /* in which level does this node lie? */
  string hkey;		    /* hash key string. */
  HashNode *parent;	    /* parent of node. */
  vector<HashNode*> children; 	/* children of node. */
  map<Itemset, int> item_sets;	/* item sets. */

 public:
  HashNode();
  HashNode(const HashNode& node);
  HashNode& operator=(const HashNode& node);
  ~HashNode(){children.clear(); item_sets.clear();}
  /* getters. */
  HashNode* getParent() const {return parent;}
  int getNumChildren() const {return children.size();}
  int getNumFreqSets() const {return item_sets.size();}
  int getNodeLevel() const {return level;}
  string getHashKey() {return hkey;}
  void visit();
  vector<HashNode*>& getChildren() {return children;}
  map<Itemset, int>& getFreqsets() {return item_sets;}

  /* setters. */
  void setParent(HashNode* p) {parent = p;}
  void setNodeLevel(int l) {level = l;}
  void setHashKey(string key) {hkey = key;}

  bool addChild(HashNode *child);
  bool insertFreqSet(Itemset& set, int cnt){item_sets[set] = cnt;}
  bool removeFreqSet(Itemset& set);
  bool findFreqSet(Itemset& set);

  /**
   * @brief Join item sets within the current node. 
   * @param sets A vector of joined sets.
   * @return true on success and false on failure.
   */
  bool joinSameParentSets(vector<Itemset>& sets);
};

class HashTree {
 private: 
  int height; 			/* Height of tree. */
  int num_nodes;		/* total number of nodes in tree. */
  HashNode *root; 		/* root of tree. */
  vector<HashNode*> kindex;	/* level k node index of tree. */
  vector<HashNode*> k1index;	/* level k+1 node index of tree. */

 public:
  HashTree();
  ~HashTree();

  /* getters */
  int getHeight() const {return height;}
  int getNumNodes() const {return num_nodes;}
  HashNode* getRoot() {return root;}
  vector<HashNode*>& getKindex() {return kindex;}
  vector<HashNode*>& getK1index() {return k1index;}
  int getKindexSize() const {return kindex.size();}
  int getK1indexSize() const {return k1index.size();}

  /* setters. */
  void setHeight(int ht) {height = ht;}
  void setNumNodes(int num) {num_nodes = num;}
  void addNodeToIndex(HashNode* node){getKindex().push_back(node);}

  /**
   * @brief Insert a node into the hash tree structure. 
   * @param parent The parent of the inserted node.
   * @param node The node to be inserted.
   * @return true on success and false on failure.
   */
  bool insertNode(HashNode *parent, HashNode *node);
  bool insertItemset(Itemset& set);

  /**
   * @brief Do join the pruning of item sets within a level. 
   * @param level Level of tree to work on. 
   * @return true on success and false on failure.
   */
  bool doJoinGrow(int level);
  /**
   * @brief Traverse tree in level order, mainly for debugging
   * purposes.
   * @param level The level to traverse. 
   * @return true on success and false on failure.
   */
  bool levelTraverse(HashNode* root);

  /**
   * @brief Print all the content of the whole tree.
   */
  void printTree();
};

#endif //ifdef
