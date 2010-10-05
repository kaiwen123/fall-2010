// -*- C -*-
/**
 * @file GeneFeatureItem.h
 * @brief Definition of a GeneFeatureItem class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#ifndef _GeneFeatureItemClass_
#define _GeneFeatureItemClass_ 
#include "defs.h"

using namespace std; 

class GeneFeatureItem {
 private: 
  gene_feature_t g_feature; 	/* Gene feature value */
  gene_class_t g_class; 	/* Anotated gene class value */
  gene_group_t g_group; 	/* Group information 'a,b,c,d'*/

 public:
  GeneFeatureItem(gene_feature_t g_feature, 
		  gene_class_t g_class); 
  ~GeneFeatureItem(){}
  
  /* getter */
  gene_feature_t getFeature()const{return g_feature;}
  gene_class_t getClass()const{return g_class;}
  gene_group_t getGroup()const{return g_group;}

  /* setter */
  void setFeature(gene_feature_t f){g_feature = f;}
  void setClass(gene_class_t c){g_class = c;}
  void setGroup(gene_group_t g){g_group = g;}

  friend ostream& operator<<(ostream& out, GeneFeatureItem const& item); 
};

#endif //ifdef
