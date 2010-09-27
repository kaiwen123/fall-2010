// -*- C++ -*-
/**
 * @file GeneDataSet.h
 * @brief Definition of a GeneDataSet class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#ifndef _GeneDataSetClass_
#define _GeneDataSetClass_ 
#include <iostream>
#include <fstream>
#include "defs.h"
#include "GeneFeatureData.h"

using namespace std; 

class GeneDataSet {
 private: 
  vector<GeneFeatureData> f_sets; /* Feature sets. */

 public:
  //GeneDataSet():num_features(0){}
  GeneDataSet(int num_f);	/* Constructor with number of features. */
  ~GeneDataSet(){f_sets.clear();}

  int getNumFeatures() const {return f_sets.size();}
  int getNumRows() const {return f_sets.at(0).getTotalCount();}
  // Operations with the data.
  /**
   * @brief Insert data into gene vector. 
   * @param f_id id number of feature. 
   * @param f_data data to be inserted into the gene feature data set. 
   * @param g_class class that this gene belongs to. 
   * @return true on success and false on failure. 
   */
  bool insertData(int f_id, gene_feature_t f_data, gene_class_t g_class); 

  /**
   * @brief Print data of the whole gene data set. 
   * @param None.
   * @output void. 
   */
  void print();

  /**
   * @brief Print data of one feature for the gene data. 
   * @param feature id.
   * @output void. 
   */
  void print(int id); 

  /**
   * @brief Print data by row. 
   * @param row number. 
   * @output void. 
   */
  void printRow(int row); 
  friend ostream& operator<<(ostream& out, GeneDataSet& g);

  /**
   * @brief Print data by row. 
   * @param row number. 
   * @output void. 
   */
  void printSize();

  /**
   * @brief Do equi-width binning to a feature data set. 
   * @param id feature id.
   * @param num_bins Total number of bins. 
   * @output True on success and false on failure. 
   */
  bool doEquiWidthBin(int id, int num_bins); 

  /**
   * @brief Print data of one feature for the gene data. 
   * @param feature id.
   * @output True on success and false on failure. 
   */
  bool doEntropyDiscretize(int id, int num_bins);

  // Saving data into files. 
  /**
   * @brief Save the equi-width discretized data into file. 
   * Row Format: f1,f2,f3,...,fn,cx
   * f1 f2 f3 ... fn are discretized features of a row. 
   * cx is the class value for this row. 
   *
   * @param none.
   * @output none.
   */
  void saveEquiWidthData(); 

  /**
   * @brief Save equi-width bins associated with this data set into file. 
   * Format of bin data: (bin_1_lb,bin_1_ub] bin_1_count, ..., one
   * line for each bin. 
   * @param none. 
   * @return none.
   */
  void saveEquiWidthBins(); 

  /** 
   * @brief Save Entropy discretized data and bins into files. 
   * The format of the data should be the same as the equi-width
   * methods.
   */
  void saveEntropyData(); 
  void saveEntropyBins();
};

#endif //ifdef
