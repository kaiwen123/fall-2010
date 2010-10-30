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
//#include "DataStore.h"

using namespace std; 

class DataSet {
 private: 
  int gnum;			/* number of genes. */
  int tnum;			/* number of transactions. */
  vector<vector<Item> > d_sets; /* Feature sets. */

 public:
  DataSet(){}
 DataSet(int g, int t):gnum(g),tnum(t){} /* Default constructor. */
  ~DataSet(){}

  /* getters. */
  int getNumGenes() const {return gnum;}
  int getNumTrans() const {return tnum;}

  /* Setters. */


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
   * @param none.
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
   * @brief Save frequent itemsets into given file.
   * @param fname The name of file to save to.
   * @return true on success and false on failure. 
   */
  bool saveFreqItemSets(string fname);

  /**
   * @brief Insert data into gene vector. 
   * @param row_num row number of inserted item.
   * @param col_num column number of inserted item.
   * @param str item information.
   * @return true on success and false on failure. 
   */
  bool insertData(int row_num, int col_num, string str); 

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
