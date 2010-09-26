#include "GeneFeatureItem.h"

// Constructor. 
GeneFeatureItem::GeneFeatureItem(gene_feature_t g_f,
				 gene_class_t g_c) {
  g_feature = g_f; 
  g_class = g_c;
  g_group = 'u';		// unknow group. 
}

// print the content of this item. 
void GeneFeatureItem::print() {
  cout << g_feature << "\t" << g_group << "\t" << g_class << endl; 
}

// Overloading << operator. 
ostream& operator<<(ostream& out, GeneFeatureItem const& item) {
  out << item.getGroup() << ","; 
  return out;
}
