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
#include "defs.h"
#include "AssoRule.h"

using namespace std; 
class DataSet {
 private: 
  vector<vector<Item> > d_sets; /* Feature sets. */
  map<Itemset, int> freqsets;	/* All frequent item sets. */
  priority_queue<AssoRule> qrules; /* Association rules. */
  /* The size of queue should be equal or less than k. */

 public:
  DataSet(){}
  ~DataSet(){}

  /* Scanners. */
  void scanLevelOne(Itemset set);
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
   * @brief Generate Association rule from Itemset. 
   */
  bool genAssoRule();

  /**
   * @brief Print top k association rules. 
   */
  void printAssoRule(); 
};

#endif //ifdef
