#include "GeneDataSet.h"
#include <cstdio> 		/* for sprintf() */

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
  char buf[6]; sprintf(buf, "%d", f_id);
  if(f_sets.at(f_id).getFid() == "")
    f_sets.at(f_id).setFid("G"+string(buf));
  f_sets.at(f_id).insert(f_data, g_class); 
  return true; 
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
// This operator will output everything to output stream. 
ostream& operator<<(ostream& out, GeneDataSet& g) {
  int col_count = g.f_sets.size();
  for(int i = 0; i < g.getNumRows(); i++) {
    gene_class_t c; 
    for(int j=0; j<col_count-1; j++){
      c = g.f_sets.at(j).getFData().at(i).getClass();
      out << g.f_sets.at(j).getFData().at(i);
    }
    out << c << endl;
  }
  return out;
}

// Do equi-width binning on a particular feature. 
bool GeneDataSet::doEquiWidthBin(int id, int num_bins) {
  return f_sets.at(id).equiWidthBinning(num_bins); 
}

// Do entropy based discretization. 
bool GeneDataSet::doEntropyDiscretize(int id, int num_bins) {
  return f_sets.at(id).entropyDiscretize(num_bins); 
}

// Save data and bins. 
void GeneDataSet::saveEquiWidthData(string fname, int k){saveData(fname, k);}
void GeneDataSet::saveEquiWidthBins(string fname, int k){
  saveBins(fname, k, 'w');
}
void GeneDataSet::saveEntropyData(string fname, int k){saveData(fname, k);}
void GeneDataSet::saveEntropyBins(string fname, int k){
  saveBins(fname, k, 'e');
}

// Save k gene data to file. 
void GeneDataSet::saveData(string fname, int k) {
  ofstream fout(fname.c_str());
  int col_count = k;
  for(int i = 0; i < getNumRows(); i++) {
    gene_class_t c; 
    for(int j = 0; j < col_count; j++){
      c = f_sets.at(j).getFData().at(i).getClass();
      fout << f_sets.at(j).getFData().at(i) << ",";
    } // for cols. 
    fout << c << endl;
  } // for rows. 
  fout.close();
}

// Save k gene bins to file. 
void GeneDataSet::saveBins(string fname, int k, char method) {
  vector<GeneFeatureData>::iterator _it = f_sets.begin(); 
  int num = 0;
  ofstream saveFile(fname.c_str());
  while(_it != f_sets.end()) {
    saveFile << _it->getFid() << ": ";
    if(method=='w') {
      vector<GeneFeatureBins>::iterator
	_bit=_it->getEquiWidthBins().begin();
      while(_bit != _it->getEquiWidthBins().end()) {
	saveFile << *_bit << ", "; 
	_bit++;
      } // while bins of gene data.
    } else if (method=='e') {
      vector<GeneFeatureBins>::iterator
	_bit=_it->getEntropyBins().begin();
      while(_bit != _it->getEntropyBins().end()) {
	saveFile << *_bit << ", "; 
	_bit++;
      } // while bins of gene data.
    } else {cerr << "" << endl; return;}
    saveFile << endl;
    _it++; num++; 
    if(num >= k) break; 
  } // while gene sets. 
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

