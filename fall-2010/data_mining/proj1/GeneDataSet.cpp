#include "GeneDataSet.h"
#include <cstdio> 		/* for sprintf() */
#include <algorithm>		// for sort algorithm

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
void GeneDataSet::saveEquiWidthData(string fname, int k){
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

void GeneDataSet::saveEquiWidthBins(string fname, int k){
    vector<GeneFeatureData>::iterator _it = f_sets.begin(); 
  int num = 0;
  ofstream saveFile(fname.c_str());
  while(_it != f_sets.end()) {
    saveFile << _it->getFid() << ": ";
    vector<GeneFeatureBins>::iterator
      _bit=_it->getEquiWidthBins().begin();
    while(_bit != _it->getEquiWidthBins().end()) {
      saveFile << *_bit << ", "; 
      _bit++;
    } // while bins of gene data.
    saveFile << endl;
    _it++; num++; 
    if(num >= k) break; 
  } // while gene sets. 
  saveFile.close();
}
void GeneDataSet::saveEntropyData(string fname, int k){
  findTopkGene(k);

#ifdef DEBUG_DATA_SET
  vector<int>::iterator it = k_highest.begin(); 
  while(it != k_highest.end()) {
    cout << *it 
  	 << "-" << f_sets.at(*it).getInfoGain() << " "; 
    it++;
  } cout << endl;
#endif
  // output k-highest data into file. 
  ofstream fout(fname.c_str());
  for(int i = 0; i < getNumRows(); i++) {
    gene_class_t c; 
    for(int j = 0; j < k; j++){
      int idx = k_highest.at(j); 
      //cout << idx << endl; 
      c = f_sets.at(idx).getFData().at(i).getClass();
      fout << f_sets.at(idx).getFData().at(i) << ",";
    } // for cols. 
    fout << c << endl;
  } // for rows. 
  fout.close();
}

void GeneDataSet::saveEntropyBins(string fname, int k){
  ofstream saveFile(fname.c_str());

  for(int i = 0; i < k; i++){
    int idx = k_highest.at(i);
    saveFile << f_sets.at(idx).getFid() << ": "; 
    vector<GeneFeatureBins>::iterator
      _bit=f_sets.at(idx).getEntropyBins().begin();
    while(_bit != f_sets.at(idx).getEntropyBins().end()) {
      saveFile << *_bit << ", "; 
      _bit++;
    } // while bins of gene data.
    saveFile << endl;
  }
  saveFile.close();
}

// Find k genes with highest information gain. 
// And put the index number into the vector k_highest. 
void GeneDataSet::findTopkGene(int k) {
  // Put the first k gene index into vector.
  k_highest.clear();
  for(int i = 0; i < k; i++)
    k_highest.push_back(i);

#ifdef DEBUG_DATA_SET
  cout << "Initial content of vector. " << endl;
  vector<int>::iterator it = k_highest.begin(); 
  while(it != k_highest.end()) {
    cout << *it 
  	 << " - " << f_sets.at(*it).getInfoGain() << " ; "; 
    it++;
  } cout << endl; 
#endif
  // Scan through the rest of the feature list, and for each item,
  // replace the smallest one in the k_highest list with this one if it
  // is larger than the smallest one. 
  for(int i = k; i < getNumFeatures(); i++) {
    int smallest = 0; 		// pointing to k_highest; 
    int smlidx = k_highest.at(smallest); // pointing to f_sets.
#ifdef DEBUG_DATA_SET
    print out current k_highest vector info gain. 
    vector<int>::iterator it = k_highest.begin(); 
    while(it != k_highest.end()) {
      cout << *it << " - " << f_sets.at(*it).getInfoGain() << " ; "; 
      it++;
    }
    cout << endl; 
#endif
    // find smallest within the inital vector.
    for(int j = 0; j < k; j++) {
#ifdef DEBUG_K_TOP
      cout << f_sets.at(smlidx).getInfoGain() << " "
      	   << f_sets.at(j).getInfoGain() << endl;
#endif
      int idx = k_highest.at(j);
      if(f_sets.at(smlidx) > f_sets.at(idx)) {
  	smallest = j; smlidx = idx; 
#ifdef DEBUG_K_TOP
	cout << "Replace small: " << idx 
	     << f_sets.at(smallest).getInfoGain()
	     << endl;	
	cout << "Current smallest in k_highest is : " 
	     << smallest << " infogain: " 
	     << f_sets.at(smallest).getInfoGain() << endl;	
#endif
      }
    }
    if(f_sets.at(i)>f_sets.at(smlidx)) {
#ifdef DEBUG_K_TOP      
      // print list before replace. 
      vector<int>::iterator it = k_highest.begin(); 
      while(it != k_highest.end()) {
      	cout << *it 
      	     << "-" << f_sets.at(*it).getInfoGain() << " "; 
      	it++;
      }
#endif
      k_highest.at(smallest) = i; // replacing ...
#ifdef DEBUG_K_TOP
      cout << "\nreplace: " << i 
      	   << " " << f_sets.at(i).getInfoGain() << endl;
      //print list after replace. 
      it = k_highest.begin(); 
      while(it != k_highest.end()) {
      	cout << *it << "-" << f_sets.at(*it).getInfoGain() << " "; 
      	it++;
      } 
      cout << endl << endl;
#endif      
    } // if
  }
  sort(k_highest.begin(), k_highest.end());
}

void GeneDataSet::printInfoGain() {
  vector<GeneFeatureData>::iterator _it = f_sets.begin();
  while(_it != f_sets.end()) {
    _it->printInfoGain();
    _it++;
  }
}
