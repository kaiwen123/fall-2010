#include "GeneFeatureBins.h"

// Constructor. 
GeneFeatureBins::GeneFeatureBins(gene_feature_t low, 
				 gene_feature_t high) {
  b_low = low; 
  b_high = high; 
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
  float crate = 0.0;
  return crate; 
}

// Information split gain. 
float GeneFeatureBins::entropy() {
  float entropy = 0.0; 
  return entropy;
}
// 

// Print the content of this bin. 
void GeneFeatureBins::print() {
  cout << "[" << b_low << ", " << b_high << ") " << getItemCount() << endl; 
}
