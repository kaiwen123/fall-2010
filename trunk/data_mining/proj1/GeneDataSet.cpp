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

// Print data by row. 
void GeneDataSet::printRow(int row) {
  int max_row = f_sets.at(0).getFData().size();
  int col_count = f_sets.size();
  gene_class_t c; 
  if((row<0) || (row>max_row)) {cerr<<"error"<<endl;return;}
  for(int i=0; i<col_count-1; i++){
    c = f_sets.at(i).getFData().at(row).getClass();
    cout << f_sets.at(i).getFData().at(row); //.print(); 
  }
  cout << c << endl; 
}
// Overload << operator for gene_class_t. 
std::ostream& operator<<(std::ostream& out, const gene_class_t& c) {
  switch(c) {
  case positive: out << "positive"; break; 
  case negative: out << "negative"; break; 
  }
  return out;
}

// Overloading operator <<. 
ostream& operator<<(ostream& out, GeneDataSet& g) {
  int col_count = g.f_sets.size();
  for(int i = 0; i < g.getNumRows(); i++) {
    gene_class_t c; 
    for(int j=0; j<col_count-1; j++){
      c = g.f_sets.at(j).getFData().at(i).getClass();
      out << g.f_sets.at(j).getFData().at(i); //.print(); 
    }
    out << c << endl;
  }
  return out;
}

// Print size of each column. 
void GeneDataSet::printSize() {
  int i = 0;
  vector<GeneFeatureData>::iterator _it = f_sets.begin();
  while(_it != f_sets.end()) {
    cout << i << " " << _it->getFData().size() << endl; 
    i++; _it++;
  }
}

// Do equi-width binning on a particular feature. 
bool GeneDataSet::doEquiWidthBin(int id, int num_bins) {
  return f_sets.at(id).equiWidthBinning(num_bins); 
}

// Do entropy based discretization. 
bool GeneDataSet::doEntropyDiscretize(int id, int num_bins) {
  return f_sets.at(id).entropyDiscretize(num_bins); 
}

// Save equi-with discretized data into file. 
// This function is deprecated. 
void GeneDataSet::saveEquiWidthData() {
  string fname("ewidth.data"); 
  ofstream saveFile(fname.c_str()); 
  saveFile.close();
}

// Save Equi-width bins into file. 
void GeneDataSet::saveEquiWidthBins() {
  vector<GeneFeatureData>::iterator _it = f_sets.begin(); 
  string fname("ewidth.bins");
  ofstream saveFile(fname.c_str());
  while(_it != f_sets.end()) {
    vector<GeneFeatureBins>::iterator _bit = _it->getEquiWidthBins().begin();
    while(_bit != _it->getEquiWidthBins().end()) {
      saveFile << *_bit; 
      _bit++;
    }
    saveFile << endl;
    _it++;
  }
  saveFile.close();
}

// Save entropy discretized data into file. 
void GeneDataSet::saveEntropyData(){
}

// Save entropy discretized bins into file. 
void GeneDataSet::saveEntropyBins(){
  vector<GeneFeatureData>::iterator _it = f_sets.begin(); 
  string fname("entropy.bins");
  ofstream saveFile(fname.c_str());
  while(_it != f_sets.end()) {
    vector<GeneFeatureBins>::iterator _bit = _it->getEntropyBins().begin();
    while(_bit != _it->getEntropyBins().end()) {
      saveFile << *_bit; 
      _bit++;
    }
    saveFile << endl;
    _it++;
  }
  saveFile.close();
}

// Find the top k genes. 
void GeneDataSet::findTopkGene(int k) {
  vector<GeneFeatureData>::iterator _it = f_sets.begin();
  while(_it != f_sets.end()) {
    cout << _it->getEntropySplit() << " " << _it->getInfoGain() << endl;
    _it++;
  }
}

