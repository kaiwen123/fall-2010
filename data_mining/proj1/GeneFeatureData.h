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
  vector<GeneFeatureItem> f_data; /* feature list head */

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
  bool equiDensityBinning(int num_bins);
  bool entropyDiscretize();
};

#endif //ifdef
