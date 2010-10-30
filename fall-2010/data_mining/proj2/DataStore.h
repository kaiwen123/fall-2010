// -*- C -*-
/**
 * @file DataStore.h
 * @brief Definition of a GeneData and TransData classes. 
 * The GeneData class is the column representation of the gene data set.
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.99
 */
// $Log$

#ifndef _DataStoreClass_
#define _DataStoreClass_ 
#include <iostream>
#include <vector>
#include "defs.h"
#include "Item.h"

using namespace std; 

class GeneData {
 private: 
  string fid;			  /* id info of data like G10 etc. */
  int p_count;			  /* positive data count. */
  int n_count;			  /* negative data count. */
  vector<Item> f_data; /* feature data vector. */

 public:
  GeneData();
  ~GeneData(){f_data.clear();} 

  /* getters. */
  int getTotalCount() const {return p_count+n_count;} 
  string getFid(){return fid;}	/* gene feature id. */
  vector<Item>& getFData() {return f_data;}

  /* setters. */
  void setFid(string id) {fid = id;} /* set feature id. */
  Item* getGeneFeature(); /* return feature object */

  void clear();			      /* Clear the feature list */
  bool insert(gene_feature_t f_data, gene_class_t g_class);
  bool find(Item& feature); /* Find a feature */
  bool isEmpty() const; 

  /**
   * @brief overloading the operator <<. 
   * @param out output stream. 
   * @param d GeneData object. 
   * @output output stream. 
   */
  friend ostream& operator<<(ostream& out, GeneData& d);

  /**
   * @brief Overloading the >= operator. 
   * This operator is used to compare two GeneData objects
   * according to their information gain. 
   * @param d rhs operant. 
   * @return true if lhs has higher information gain than the rhs. 
   * @return false if lhs has smaller info-gain than rhs.
   */
  bool operator>(GeneData& d);
  void printInfoGain(); 
};

/**
 * @brief TransData class. 
 * This class is used to store the transactional data for the purpose
 * of scanning during the itemset generationg process. 
 */
class TransData {
 private:

 public:
};

//bool operator<(GeneData& a, GeneData& b);
#endif //ifdef
