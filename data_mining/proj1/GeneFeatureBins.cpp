#include "GeneFeatureBins.h"

// Constructor. 
GeneFeatureBins::GeneFeatureBins(gene_feature_t low, 
				 gene_feature_t high) {
  b_low = low; 
  b_high = high; 
  b_group = 'u';		// unknow group.
  p_count = 0; 
  n_count = 0;
}

// Insert a feature item into this bin. 
void GeneFeatureBins::insertItem(GeneFeatureItem& f_i) {
  gene_class_t cls = f_i.getClass(); 
  //g_f_items.push_back(f_i); // can be removed. 
  //cout << "Setting group information for : "; 
  //f_i.print(); 
  f_i.setGroup(b_group); 
  if(cls == positive)
    p_count++; 
  else if(cls == negative)
    n_count++; 
}

// calculate the consistency rate of this bin.
float GeneFeatureBins::cRate() {
  //cout << p_count << " " << n_count << endl;
  return (p_count > n_count) ? ((float)p_count/getTotalCount()) 
    : ((float)n_count/getTotalCount());
}

// Information split gain. 
float GeneFeatureBins::entropy() {
  return (-((double)p_count/getTotalCount())*log2((double)p_count/getTotalCount())
	  -((double)n_count/getTotalCount())*log2((double)n_count/getTotalCount()));
}
// 

// Print the content of this bin. 
void GeneFeatureBins::print() {
  cout << "[" << b_low << ", " << b_high << ") " << getTotalCount() << endl; 
}

// Overloading << operator. 
ostream& operator<<(ostream& out, GeneFeatureBins& b) {
  out << "[" << b.b_low << ", " << b.b_high << ") " << b.getTotalCount() << ", "; 
  return out;
}
