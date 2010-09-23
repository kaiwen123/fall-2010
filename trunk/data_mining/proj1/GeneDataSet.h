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

  int getNumFeatures(){return f_sets.size();}
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
};

#endif //ifdef
