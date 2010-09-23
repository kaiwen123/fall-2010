#include "GeneFeatureData.h"

GeneFeatureData::GeneFeatureData() {
}

// Inserting data into the gene feature data. 
bool GeneFeatureData::insert(gene_feature_t f_dat, 
			     gene_class_t g_c) {
  // Building gene feature data item; 
  GeneFeatureItem* pFeatureItem = new GeneFeatureItem(f_dat, g_c); 
  if(!pFeatureItem) {
    cerr << "Can't create new GeneFeatureItem object. " << endl; 
    return false; 
  }
  f_data.push_back(*pFeatureItem); 
  return true; 
}

// Print the content of this feature data. 
void GeneFeatureData::print() {
  vector<GeneFeatureItem>::iterator it = f_data.begin(); 
  while(it != f_data.end()) {
    it->print(); 		// print the data for feature item. 
    it++;
  }
}
