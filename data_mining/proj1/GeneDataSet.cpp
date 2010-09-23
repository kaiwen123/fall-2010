#include "GeneDataSet.h"

// Constructor with number of features. 
GeneDataSet::GeneDataSet(int num_f) {
  num_features = num_f; 
  for(int i = 0; i < num_f; i++) {
    GeneFeatureData f_data; 
    f_sets.push_back(f_data); 
  } // for loop
  //cout << "hello" << endl; 
}

// 
bool GeneDataSet::insertData(int f_id, 
			     gene_feature_t f_data, 
			     gene_class_t g_class) {
  f_sets.at(f_id).insert(f_data, g_class); 
  return true; 
} 
