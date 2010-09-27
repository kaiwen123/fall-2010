// -*- C++ -*-
/**
 * @file GeneFeatureData.h
 * @brief Definition of a GeneFeatureData class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.99
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
  gene_feature_t entropy_split;	  /* entropy split. */
  int p_count;			  /* positive data count. */
  int n_count;			  /* negative data count. */
  float info_gain;		  /* information gain for entropy. */
  vector<GeneFeatureItem> f_data; /* feature data vector. */
  vector<GeneFeatureBins> f_width_bins; /* equi-width bins. */
  vector<GeneFeatureBins> f_entropy_bins; /* entropy based bins. */

 public:
  GeneFeatureData();
  ~GeneFeatureData(){f_data.clear();} 

  int getTotalCount() const {return p_count+n_count;} 
  gene_feature_t getEntropySplit() const {return entropy_split;}
  float getInfoGain() const {return info_gain;}
  vector<GeneFeatureItem>& getFData() {return f_data;}
  vector<GeneFeatureBins>& getEquiWidthBins() {return f_width_bins;}
  vector<GeneFeatureBins>& getEntropyBins() {return f_entropy_bins;}

  GeneFeatureItem* getGeneFeature(); /* return feature object */
  void clear();			      /* Clear the feature list */
  //bool insert(GeneFeatureItem& feature);
  bool insert(gene_feature_t f_data, gene_class_t g_class);
  void print();			       /* print the feature set */
  bool find(GeneFeatureItem& feature); /* Find a feature */
  bool isEmpty() const; 

  // Binning Methods.
  /**
   * @brief Equi-width binning function.
   * @param number of bins. 
   * @return true on success and false on failure. 
   */
  bool equiWidthBinning(int num_bins);

/**
   * @brief Entropy based binning function.
   * @param none.  
   * @return true on success and false on failure. 
   */
  bool entropyBestSplit(int num_bins);

  /**
   * @brief Entropy based binning function.
   * @param none.  
   * @return true on success and false on failure. 
   */
  bool entropyDiscretize(int num_bins);

  /**
   * @brief Get dataset entropy. 
   * @param none. 
   * @return Calculated entropy for feature data. 
   */
  float calcDataEntropy();

  /**
   * @brief Calculate the information of the split. 
   * The information of the split can be calculated accordong to: 
   * IS(S1,S2) = (|S1|/|S|)Entropy(S1) + (|S2|/|S|)Entropy(S2). 
   *
   * @param none. 
   * @return Calculated entropy for the splited feature data. 
   */
  float calcInfoSplit(); 

  /**
   * @brief Calculate the information of the split compared with 
   * the original information according to formula below: 
   * Gain(v, S) = EntropyS() - IS(S1, S2).
   *
   * @param none. 
   * @return Calculated information gain of the split. 
   */
  float calcInfoGain(); 

  // Printing and output methods. 
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

  /**
   * @brief Print the content of the entropy bins. 
   * @param None.
   * @output void. 
   */
  friend ostream& operator<<(ostream& out, GeneFeatureData& d);
};

#endif //ifdef
