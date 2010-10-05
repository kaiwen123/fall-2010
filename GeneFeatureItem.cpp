#include "GeneFeatureItem.h"

// Constructor. 
GeneFeatureItem::GeneFeatureItem(gene_feature_t g_f,
				 gene_class_t g_c) {
  g_feature = g_f; 
  g_class = g_c;
  g_group = 'u';		// unknow group. 
}

// Overloading << operator. 
ostream& operator<<(ostream& out, GeneFeatureItem const& item) {
  out << item.getGroup();// << ","; 
  return out;
}
