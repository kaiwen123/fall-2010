#include "GeneDataSet.h"

// Constructor with number of features. 
GeneDataSet::GeneDataSet(int num_f) {
  for(int i = 0; i < num_f; i++) {
    GeneFeatureData f_data; 
    f_sets.push_back(f_data); 
  } // for loop
}

// Insert data into the data set. 
bool GeneDataSet::insertData(int f_id, 
			     gene_feature_t f_data, 
			     gene_class_t g_class) {
  f_sets.at(f_id).insert(f_data, g_class); 
  return true; 
} 

// Print the content of the data set. 
void GeneDataSet::print() {
  vector<GeneFeatureData>::iterator it = f_sets.begin(); 
  while(it != f_sets.end()) {
    it->print(); 
    it++;
  }
}

// Print the content of a particular feature. 
void GeneDataSet::print(int id) {
  f_sets.at(id).print(); 
}
