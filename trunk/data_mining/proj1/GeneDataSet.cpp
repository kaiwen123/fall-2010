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
  if((row<0) || (row>max_row)) {cerr<<"error"<<endl;return;}
  for(int i=0; i<col_count; i++){
    f_sets.at(i).getFData().at(row).print(); 
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
void GeneDataSet::saveEquiWidthData() {
  string fname("ewidth.data"); 
  ofstream saveFile(fname.c_str()); 

  // print data line by line. 
  for(int i = 0; i < f_sets.at(0).getTotalCount(); i++) {
    //cout << f_sets.at(0).getTotalCount() << endl;
    for(int j = 0; j < getNumFeatures(); j++) {
      //saveFile << f_sets.at(j).getFData().at(i);
      cout << f_sets.at(j).getFData().at(i);
    }
    // vector<GeneFeatureData>::iterator _dsit = f_sets.begin();//feature set.
    // while(_dsit != f_sets.end()) {
    //   //saveFile << _dsit->getFData().at(1);
    //   cout << _dsit->getFData().at(1) << i << endl;
    //   _dsit++;
    // }
  }
  // vector<GeneFeatureData>::iterator _dsit = f_sets.begin();//feature set. 
  // vector<GeneFeatureItem>::iterator _dit = _dsit->getFData().begin(); //feature item.
  // while(_dit != _dsit->getFData().end()) {
  //   while(_dsit != f_sets.end()) {
  //     //saveFile << *_dit; 
  //     //cout << *_dit;
  //     _dsit++;
  //   } //f_sets while. 
  //   _dit++; 
  // } //while item. 
  saveFile.close();
}
// Save Equi-width bins into file. 
void GeneDataSet::saveEquiWidthBins() {

}

// Save entropy discretized data into file. 
void GeneDataSet::saveEntropyData(){
}

// Save entropy discretized bins into file. 
void GeneDataSet::saveEntropyBins(){
}

