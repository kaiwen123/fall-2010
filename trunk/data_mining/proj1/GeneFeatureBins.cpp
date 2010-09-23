#include "GeneFeatureBins.h"

// Constructor. 
GeneFeatureBins::GeneFeatureBins(gene_feature_t low, 
				 gene_feature_t high) {
  b_low = low; 
  b_high = high; 
  p_count = 0; 
  n_count = 0;
}

// Insert a feature item into this bin. 
void GeneFeatureBins::insertItem(GeneFeatureItem feature) {
  // cout << "Insert item into bin ...  " << feature.getFeature()
  //      << "\t" << feature.getClass() << endl; 
  gene_class_t cls = feature.getClass(); 
  g_f_items.push_back(feature); 
  cout << "current size of the bin: " << getItemCount() << endl; 
  if(cls == positive)
    p_count++; 
  else if(cls == negative)
    n_count++; 
}

// calculate the consistency rate of this bin.
float GeneFeatureBins::cRate() {
  cout << p_count << " " << n_count << endl;
  return (p_count > n_count) ? ((float)p_count/getItemCount()) 
    : ((float)n_count/getItemCount());
}

// Information split gain. 
float GeneFeatureBins::entropy() {
  return (-((double)p_count/getItemCount())*log2((double)p_count/getItemCount())
	  -((double)n_count/getItemCount())*log2((double)n_count/getItemCount()));
}
// 

// Print the content of this bin. 
void GeneFeatureBins::print() {
  cout << "[" << b_low << ", " << b_high << ") " << getItemCount() << endl; 
}
