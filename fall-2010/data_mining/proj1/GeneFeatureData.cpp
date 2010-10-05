#include "GeneFeatureData.h"

GeneFeatureData::GeneFeatureData() {
  fid = "";
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
  // refresh the high and low boundary of the gene data sample set. 
  if(f_dat>f_highest) f_highest=f_dat; 
  if(f_dat<f_lowest) f_lowest=f_dat;

  // update the positive and negative data counters.
  if(g_c == positive) p_count++; 
  else n_count++;
  return true; 
}

// Do equi-width binning. 
bool GeneFeatureData::equiWidthBinning(int num_bins) {
  float interval = (f_highest - f_lowest) / num_bins; 
  // Creating bins. 
  for(int i = 0; i < num_bins; i++) {
    gene_feature_t blow = f_lowest+i*interval; 
    gene_feature_t bhigh = f_lowest+(i+1)*interval; 
    GeneFeatureBins bin(blow, bhigh);
    bin.setMinInf(0.0); bin.setMaxInf(f_highest);
    bin.setGroup('a'+i);	// Group information a,b,c,d....;
    vector<GeneFeatureItem>::iterator _iit = f_data.begin();
    while(_iit != f_data.end()) {
      gene_feature_t fvar = _iit->getFeature();
      if(i==num_bins-1) bhigh += 10000;	// Last bin. 
      if((fvar >= blow) && (fvar < bhigh)){
	bin.insertItem(*_iit);
      }
      _iit++;
    }
    f_width_bins.push_back(bin); // put bin into the vector.
  } // for 
}

// Information split gain. 
float GeneFeatureData::calcDataEntropy() {
  int count = getTotalCount(); 
  float entropyp = 0.0, entropyn = 0.0;
  if(count == 0) return 0.0; 
  entropyp = (p_count == 0) ? 0.0 : -((double)p_count/count)*log2((double)p_count/count);
  entropyn = (n_count == 0) ? 0.0 :
  -((double)n_count/count)*log2((double)n_count/count);
#ifdef DEBUG_DATA_ENTROPY
  cout << "\nCalculating Data Entropy for " << getFid() << endl
       << "Entropy of data: " << "P: " << p_count << "-" << entropyp
       << "\t\tN: " << n_count << "-" << entropyn << endl; 
#endif
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
#ifdef DEBUG_INFO_GAIN
  cout << "\nCalculating info split for " << getFid() << endl
       << "Entropy S1: " << s1_size << "-" << s1_entropy
       << "\tEntropy S2: " << s2_size << "-" << s2_entropy << endl; 
#endif
    return ((double)s1_size/s_size)*s1_entropy + 
      ((double)s2_size/s_size)*s2_entropy;
  } else return 0.0;
}

// Calculate the information gain of the split. 
float GeneFeatureData::calcInfoGain() {
  float data_entropy = calcDataEntropy(); 
  float info_split = calcInfoSplit();
#ifdef DEBUG_INFO_GAIN
  cout << "\nCalculating info gain for " << getFid() << endl
       << "Data entropy: " << data_entropy
       << "\tinfo split: " << info_split
       << "\tInfo gain: " << data_entropy - info_split << endl; 
#endif
  return data_entropy - info_split;
}

// Do entropy based binning.
bool GeneFeatureData::entropyDiscretize(int num_bins) { 
  entropyBestSplit(num_bins);
  GeneFeatureBins bin1(0.0, entropy_split);
  GeneFeatureBins bin2(entropy_split, f_highest);
  bin1.setGroup('a'); bin2.setGroup('b');
  bin1.setMinInf(0.0); bin1.setMaxInf(f_highest);
  bin2.setMinInf(0.0); bin2.setMaxInf(f_highest);
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
  return true;
}

// Find the best split with highest information gain. 
bool GeneFeatureData::entropyBestSplit(int num_bins) {
#ifdef DEBUG_DATA
  cout << "Working for gene " << getFid() << endl; 
#endif
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
    } // while test all splits.
#ifdef DEBUG_DATA
    cout << "Current info gain : " << info_gain << endl;
#endif
    float tmp_infogain = calcInfoGain();
    if(info_gain < tmp_infogain){
      info_gain = tmp_infogain;
      entropy_split = v;
#ifdef DEBUG_DATA
      cout << "Change info gain to: " << info_gain << endl;
#endif
    }
    _git++;
  }//while gene data set. 
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

void GeneFeatureData::printInfoGain() {
  cout << getFid() << " - " << "split: " << entropy_split
       << " gain: " << getInfoGain() << endl; 
}

// overloading the >= operator. 
// Compare two objects according to info-gain.
bool GeneFeatureData::operator>(GeneFeatureData& d) {
  return getInfoGain() > d.getInfoGain();
}
