// -*- C++ -*-
/**
 * @file GeneFeatureItem.h
 * @brief Definition of a GeneFeatureItem class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#ifndef _GeneFeatureItemClass_
#define _GeneFeatureItemClass_ 
#include <iostream>
#include "defs.h"

using namespace std; 

class GeneFeatureItem {
 private: 
  gene_feature_t g_feature; 	/* Gene feature value */
  gene_class_t g_class; 	/* Anotated gene class value */

 public:
  GeneFeatureItem(gene_feature_t g_feature, gene_class_t g_class); 
  ~GeneFeatureItem(){}
  
  gene_feature_t getFeature(){return g_feature;}
  gene_class_t getClass(){return g_class;}

  /* printing functions */
  void print();
  friend ostream& operator<<(ostream& out, const GeneFeatureItem& item); 
};

#endif //ifdef
