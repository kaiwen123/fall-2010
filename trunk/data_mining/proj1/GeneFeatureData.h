// -*- C++ -*-
/**
 * @file GeneFeatureData.h
 * @brief Definition of a GeneFeatureData class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#ifndef _GeneFeatureDataClass_
#define _GeneFeatureDataClass_ 
#include <iostream>
#include <vector>
#include "defs.h"
#include "GeneFeatureItem.h"
#include "GeneFeatureBins.h"

using namespace std; 

class GeneFeatureData {
 private: 
  gene_feature_t f_highest;	  /* highest feature data. */
  gene_feature_t f_lowest;	  /* lowest feature data. */
  int p_count;			  /* positive class count in this gene data set. */
  int n_count;			  /* negative class count in this gene data set. */
  vector<GeneFeatureItem> f_data; /* feature list head */
  vector<GeneFeatureBins> f_width_bins; /* equi-width bins. */
  vector<GeneFeatureBins> f_entropy_bins; /* entropy based bins. */

 public:
  GeneFeatureData();
  ~GeneFeatureData(){f_data.clear();} 

  int getNumGenes() const {return f_data.size();}
  GeneFeatureItem * getGeneFeature(); /* return feature object */
  void clear();			      /* Clear the feature list */
  //bool insert(GeneFeatureItem& feature);
  bool insert(gene_feature_t f_data, gene_class_t g_class);
  void print();			       /* print the feature set */
  bool find(GeneFeatureItem& feature); /* Find a feature */
  bool isEmpty() const; 

  /* Binning Methods. */
  bool equiWidthBinning(int num_bins);
  bool entropyDiscretize();

  /**
   * @brief Print the content of the equi-width bins. 
   * @param None.
   * @output void. 
   */
  void printEWBins();

  /**
   * @brief Print the content of the entropy bins. 
   * @param None.
   * @output void. 
   */
  void printEntropyBins();
};

#endif //ifdef
