// -*- C -*-
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
  vector<int> k_highest;	  /* k-highest info gain genes index. */

 public:
  GeneDataSet(int num_f);	/* Constructor with number of features. */
  ~GeneDataSet(){f_sets.clear();}

  /* getters. */
  int getNumFeatures() const {return f_sets.size();}
  int getNumRows() const {return f_sets.at(0).getTotalCount();}

  /**
   * @brief Insert data into gene vector. 
   * @param f_id id number of feature. 
   * @param f_data data to be inserted into the gene feature data set. 
   * @param g_class class that this gene belongs to. 
   * @return true on success and false on failure. 
   */
  bool insertData(int f_id, gene_feature_t f_data, gene_class_t g_class); 

  /**
   * @brief Overloading the << operator to output stream. 
   * @param out output stream. 
   * @param g GeneDataSet object. 
   * @output out output stream. 
   */
  friend ostream& operator<<(ostream& out, GeneDataSet& g);

  /**
   * @brief Do equi-width binning to a feature data set. 
   * @param id feature id.
   * @param num_bins Total number of bins. 
   * @output True on success and false on failure. 
   */
  bool doEquiWidthBin(int id, int num_bins); 

  /**
   * @brief Do entropy based binning for gene data set. 
   * @param id feature id.
   * @param num_bins total number of bins for gene data set. 
   * @output True on success and false on failure. 
   */
  bool doEntropyDiscretize(int id, int num_bins);
 
  /**
   * @brief Save the equi-width discretized data into file. 
   * Format of data: f1,f2,f3,...,fn,cx
   * f1 f2 f3 ... fn are discretized features of a row. 
   * cx is the class value for this row. 
   *
   * Format of bins: (bin_1_lb,bin_1_ub] bin_1_count, ..., one
   * line for each bin.
   * @param fname File name to save data/bins to.
   * @param k number of genes to save. 
   * @output none.
   */
  void saveEquiWidthData(string fname, int k); 
  void saveEquiWidthBins(string fname, int k); 

  /**
   * @brief Save Entropy discretized data and bins into files. 
   * The format of the data should be the same as the equi-width
   * methods.
   * @param fname File name to save entropy data/bins to. 
   * @param k number of top gene data to save to. 
   * @return none. 
   */
  void saveEntropyData(string fname, int k); 
  void saveEntropyBins(string fname, int k);

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
