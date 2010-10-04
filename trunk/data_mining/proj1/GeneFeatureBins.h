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
#include <cmath>
#include "defs.h"
#include "GeneFeatureItem.h"

using namespace std; 

class GeneFeatureBins {
private: 
  float MININF, MAXINF;		/* -inf and +inf */
  gene_feature_t b_low; 	/* Lower boundary of bin. */
  gene_feature_t b_high; 	/* Higher boundary of bin. */
  gene_group_t b_group;		/* group of this bin. */
  int p_count; 			/* Total number of positive genes in this bin. */
  int n_count; 			/* Total number of negative genes in this bin. */
  vector<GeneFeatureItem> g_f_items; /* Feature items within this bin. */

 public:
  GeneFeatureBins(gene_feature_t low, gene_feature_t high);
  ~GeneFeatureBins(){g_f_items.clear();} 

  /* getters */
  int getPCount() const {return p_count;}
  int getNCount() const {return n_count;}
  int getTotalCount() const {return p_count + n_count;}
  gene_feature_t getLowBoundary(){return b_low;}
  gene_feature_t getHighBoundary(){return b_high;}
  gene_group_t getGroup(){return b_group;}

  /* Setters. */
  void setGroup(gene_group_t g){b_group = g;}
  void setMinInf(float min){MININF = min;}
  void setMaxInf(float max){MAXINF = max;}

  /* feature items operation. */
  void insertItem(GeneFeatureItem& feature); /* Insert feature */
  void deleteItem(GeneFeatureItem& feature); /* delete feature */
  /**
   * @brief Test if current bin contains a GeneFeatureItem. 
   * @param GeneFeatureItem object to be tested. 
   * @return true if bin contains item; 
   * false if bin doesn't contain item. 
   */
  bool contains(GeneFeatureItem& f){return (f.getFeature() >= b_low) 
      && (f.getFeature() < b_high);}

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
   * @brief Overloading the << operator for output.
   * @param None.
   * @output void. 
   */
  friend ostream& operator<<(ostream& out, GeneFeatureBins& b); 

};

#endif //ifdef
