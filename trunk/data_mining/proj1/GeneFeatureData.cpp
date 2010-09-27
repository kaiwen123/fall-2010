#include "GeneFeatureData.h"

GeneFeatureData::GeneFeatureData() {
  f_highest = 0.0; 
  f_lowest = 0.0;
  info_gain = 0.0;
  p_count = 0;
  n_count = 0;
}

// Inserting data into the gene feature data. 
bool GeneFeatureData::insert(gene_feature_t f_dat, 
			     gene_class_t g_c) {
  // Building gene feature data item and insert into vector. 
  GeneFeatureItem* pF = new GeneFeatureItem(f_dat, g_c); 
  if(!pF){cerr<<"Can't create new GeneFeatureItem object."<<endl;return false;}
  f_data.push_back(*pF); 
  //pF->print();
  // refresh the high and low boundary of the gene data sample set. 
  if(f_dat>f_highest) f_highest=f_dat; 
  if(f_dat<f_lowest) f_lowest=f_dat;

  // update the positive and negative data counters.
  if(g_c == positive) p_count++; 
  else n_count++;
  return true; 
}

// Print the content of this feature data. 
void GeneFeatureData::print() {
  vector<GeneFeatureItem>::iterator it = f_data.begin(); 
  int totalnum = 0;
  while(it != f_data.end()) {
    it->print(); 		// print the data for feature item. 
    it++;
    totalnum++;
  }
}

// Do equi-width binning. 
bool GeneFeatureData::equiWidthBinning(int num_bins) {
  float interval = (f_highest - f_lowest) / num_bins; 
  // Creating bins. 
  for(int i = 0; i < num_bins; i++) {
    gene_feature_t blow = f_lowest+i*interval; 
    gene_feature_t bhigh = f_lowest+(i+1)*interval; 
    GeneFeatureBins bin(blow, bhigh);
    //cout << "Current Bin: " << bin << "----------" << endl;
    bin.setGroup('a'+i);	// Group information a,b,c,d....;
    vector<GeneFeatureItem>::iterator _iit = f_data.begin();
    while(_iit != f_data.end()) {
      //_iit->print();
      gene_feature_t fvar = _iit->getFeature(); // data feature value. 
      if(i==num_bins-1) bhigh += 10000;	// Last bin. 
      if((fvar >= blow) && (fvar < bhigh)){
	bin.insertItem(*_iit);
      }
      _iit++;
    }
    f_width_bins.push_back(bin); // put bin into the vector.
  } // for 
  //printEWBins(); 
}

// Information split gain. 
float GeneFeatureData::calcDataEntropy() {
  int count = getTotalCount(); 
  float entropyp = 0.0, entropyn = 0.0;
  if(count == 0) return 0.0; 
  entropyp = (p_count == 0) ? 0.0 : -((double)p_count/count)*log2((double)p_count/count);
  entropyn = (n_count == 0) ? 0.0 : -((double)n_count/count)*log2((double)n_count/count);
  return entropyp + entropyn;
}

// Calculate the information of the split. 
float GeneFeatureData::calcInfoSplit() {
  if(f_entropy_bins.size()>0) {
    int s1_size = f_entropy_bins.at(0).getTotalCount(); 
    int s2_size = f_entropy_bins.at(1).getTotalCount();
    int s_size = s1_size + s2_size; 
    float s1_entropy = f_entropy_bins.at(0).entropy(); 
    float s2_entropy = f_entropy_bins.at(1).entropy();
    //cout << f_entropy_bins.at(0) << "  " << f_entropy_bins.at(1) << endl; 
    //cout << "split entropy " << s1_entropy << " " << s2_entropy << endl; 
    return ((double)s1_size/s_size)*s1_entropy + ((double)s2_size/s_size)*s2_entropy;
  } else {
    return 0.0; 
  }
}

// Calculate the information gain of the split. 
float GeneFeatureData::calcInfoGain() {
  //cout << "info gain: " << calcDataEntropy() << " " << calcInfoSplit() << endl; ;
  return calcDataEntropy() - calcInfoSplit();
}

// Print equi-width bins. 
void GeneFeatureData::printEWBins() {
  cout << "Contents of bin: " << endl; 
  vector<GeneFeatureBins>::iterator it = f_width_bins.begin(); 
  while(it != f_width_bins.end()) {
    it->print();  
    it++;
  }
}

// Do entropy based binning.
bool GeneFeatureData::entropyDiscretize(int num_bins) { 
  entropyBestSplit(num_bins);
  GeneFeatureBins bin1(0.0, entropy_split);
  GeneFeatureBins bin2(entropy_split, f_highest);
  bin1.setGroup('a'); bin2.setGroup('b');
  vector<GeneFeatureItem>::iterator _it = f_data.begin(); 
  while(_it != f_data.end()){
    if(bin1.contains(*_it)) {
      bin1.insertItem(*_it);
    } else {
      bin2.insertItem(*_it);
    }
    _it++;
  }
  f_entropy_bins.clear();
  f_entropy_bins.push_back(bin1);
  f_entropy_bins.push_back(bin2);
  //cout << f_entropy_bins.at(0) << " " << f_entropy_bins.at(1) << endl; 
  return true;
}

// Find the best split with highest information gain. 
bool GeneFeatureData::entropyBestSplit(int num_bins) {
  // For the gene data, try all possible split and get the one with
  // highest information split. 
  vector<GeneFeatureItem>::iterator _git = f_data.begin(); 
  gene_feature_t v = 0.0; 
  while(_git != f_data.end()) {
    v = _git->getFeature();
    // create two entropy bin. 
    GeneFeatureBins bin1(0.0, v);
    GeneFeatureBins bin2(v, f_highest);
    bin1.setGroup('a'); bin2.setGroup('b');
    // caculate information gain
    // First need to color all gene data. 
    vector<GeneFeatureItem>::iterator _gfit = f_data.begin(); 
    while(_gfit != f_data.end()){
      // Discretize data first. with class a, b. 
      if(bin1.contains(*_gfit)) {
	bin1.insertItem(*_gfit); 
      } else {
	bin2.insertItem(*_gfit); 
      }    
      // Current information gain is bigger.
      f_entropy_bins.clear();
      f_entropy_bins.push_back(bin1);
      f_entropy_bins.push_back(bin2); 
      _gfit++;
    }
    //float tmp_infogain=(calcInfoGain()==1) ? 0.0 :calcInfoGain();
    float tmp_infogain = calcInfoGain();
    if(info_gain < tmp_infogain){
      info_gain = tmp_infogain;
      entropy_split = v;
    }
    _git++;
  }
  return true;
}

// Overloading << operator. 
ostream& operator<<(ostream& out, GeneFeatureData& d) {
  vector<GeneFeatureItem>::iterator _dit = d.getFData().begin(); 
  while(_dit != d.getFData().end()) {
    out << *_dit; 
    _dit++;
  }
  return out; 
}
