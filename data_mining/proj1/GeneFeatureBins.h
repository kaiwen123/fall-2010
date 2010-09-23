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
#include "GeneFeatureItem.h"

using namespace std; 

class GeneFeatureBins {
 private: 
  gene_feature_t b_low; 	/* Lower boundary of bin. */
  gene_feature_t b_high; 	/* Higher boundary of bin. */
  int p_count; 			/* Total number of positive genes in this bin. */
  int n_count; 			/* Total number of negative genes in this bin. */
  vector<GeneFeatureItem> g_f_items; /* Feature items within this bin. */

 public:
  GeneFeatureBins(gene_feature_t low, gene_feature_t high);
  ~GeneFeatureBins(){g_f_items.clear();} 

  int getPCount() const {return p_count;}
  int getNCount() const {return n_count;}
  gene_feature_t getLowBoundary(){return b_low;}
  gene_feature_t getHighBoundary(){return b_high;}
  void insertItem(GeneFeatureItem feature); /* Insert feature */
  void deleteItem(GeneFeatureItem feature); /* delete feature */
  int getItemCount() const {return g_f_items.size();} /* Size of this bin. */

  /**
   * @brief Calculate the consistency rate for this bin. 
   * @param None.
   * @output The calculated consistency rate. 
   */
  float cRate();

  /**
   * @brief Calculate the Entropy for this bin. 
   * @param None.
   * @output The calculated consistency rate. 
   */
  float entropy();			      /* information split gain. */

  /**
   * @brief Print the content of this bin. 
   * @param None.
   * @output void. 
   */
  void print();
};

#endif //ifdef
