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
  f_i.setGroup(b_group); 
  if(cls == positive)
    p_count++; 
  else if(cls == negative)
    n_count++; 
}

// calculate the consistency rate of this bin.
float GeneFeatureBins::cRate() {
  return (p_count > n_count) ? ((float)p_count/getTotalCount()) 
    : ((float)n_count/getTotalCount());
}

// Information split gain. 
float GeneFeatureBins::entropy() {
  int count = getTotalCount(); 
  float entropyp = 0.0, entropyn = 0.0;
  if(count == 0) return 0.0; 
  entropyp = (p_count == 0) ? 0.0 : -((double)p_count/count)*log2((double)p_count/count);
  entropyn = (n_count == 0) ? 0.0 : -((double)n_count/count)*log2((double)n_count/count);
  return entropyp + entropyn;
}

// Overloading << operator. 
ostream& operator<<(ostream& out, GeneFeatureBins& b) {
  out << "[" << b.b_low << "," 
      << b.b_high << ")" << b.getTotalCount(); 
  return out;
}
