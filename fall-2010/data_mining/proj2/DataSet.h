// -*- C -*-
/**
 * @file DataSet.h
 * @brief Definition of a ataSet class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#ifndef _DataSetClass_
#define _DataSetClass_ 
#include <iostream>
#include <fstream>
#include "defs.h"
#include "HashTree.h"
#include <map>

using namespace std; 
class DataSet {
 private: 
  int minSup; 			/* minimum support. */
  float minConf;		/* minimum confidence. */
  int gnum;			/* total number of genes. */
  int tnum;			/* total number of transactions. */
  vector<vector<Item> > d_sets; /* Feature sets. */
  map<int, int> fst;		/* first level item sets. */
  map<Itemset, int> snd;	/* Second level item sets. */
  HashTree *hroot;		/* the root of hash tree. */

 public:
  DataSet();
  ~DataSet(){}

  /* getters. */
  int getNumGenes() const {return gnum;}
  int getNumTrans() const {return tnum;}
  int getMinSupport() {return minSup;}
  float getMinConf() {return minConf;}

  /* Setters. */
  void setMinSupport(int sup) {minSup = sup;}
  void setMinConf(float conf) {minConf = conf;}
  void setNumGenes(int g) {gnum = g;}
  void setNumTrans(int t) {tnum = t;}

  /* Scanners. */
  void scanLevelOne(int id);
  void printLevelFreqSets(int level);
  void scanLevelTwo();

  /**
   * @brief Load gene data from file.
   * @param function will prompt user to enter the file name. 
   * @return true on success and false on failure. 
   */
  bool loadFromFile(string fname);

  /**
   * @brief Do the gene Item mapping from group data into unique ids.
   * @param none.
   * @return true on success and false on failure. 
   */
  bool doItemMap(); 

  /**
   * @brief Save item map into given file. 
   * @param fname file to save mapping result to.
   * @return true on success and false on failure. 
   */
  bool saveItemMap(string fname);

  /**
   * @brief Do apriori association rule mining to gene data set.
   * @param none.
   * @return true on success and false on failure. 
   */
  bool doApriori();

  /**
   * @brief Scan item sets within the whole dataset.
   * @param set The dataset to be scanned. 
   * @return number of occurances of itemset within the dataset.
   */
  int scanItemset(Itemset& set);

  /**
   * @brief Save frequent itemsets into given file.
   * @param fname The name of file to save to.
   * @return true on success and false on failure. 
   */
  bool saveFreqItemSets(string fname);

  /**
   * @brief Overloading the << operator to output stream. 
   * @param out output stream. 
   * @param g GeneDataSet object. 
   * @output out output stream. 
   */
  friend ostream& operator<<(ostream& out, DataSet& g);

  /**
   * @brief Find k genes with highest information gain after the
   * entropy based discretization.
   * @param k the number of gene to be searched. 
   * @return none.
   */
  void findTopkGene(int k); 
  void printInfoGain();
};

#endif //ifdef
