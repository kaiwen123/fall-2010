// -*- C++ -*-
/**
 * @file GeneFeatureBins.h
 * @brief Definition of a GeneFeatureBins class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#ifndef _GeneFeatureBinsClass_
#define _GeneFeatureBinsClass_ 
#include <iostream>
#include <vector> 
#include "defs.h"

using namespace std; 

class GeneFeatureBins {
 private: 
  gene_feature_t b_low; 	/* Lower boundary of bin. */
  gene_feature_t b_high; 	/* Higher boundary of bin. */
  vector<GeneFeatureItem> g_f_items; /* Feature items within this bin. */

 public:
  GeneFeatureBins(gene_feature_t b_low, gene_feature_t b_high);
  ~GeneFeatureBins(); 

  gene_feature_t getLowBoundary(){return b_low;}
  gene_feature_t getHighBoundary(){return b_high;}
  void insertItem(GeneFeatureItem * feature); /* Insert feature */
  void deleteItem(GeneFeatureItem * feature); /* delete feature */
  int getItemCount(){return g_f_items.size();} /* Size of this bin. */
  void print();				      /* Print bin contents. */
};

#endif //ifdef
