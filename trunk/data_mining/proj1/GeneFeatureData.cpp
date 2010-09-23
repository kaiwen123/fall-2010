#include "GeneFeatureData.h"

GeneFeatureData::GeneFeatureData() {
  f_highest = 0.0; 
  f_lowest = 0.0;
  p_count = 0;
  n_count = 0;
}

// Inserting data into the gene feature data. 
bool GeneFeatureData::insert(gene_feature_t f_dat, 
			     gene_class_t g_c) {
  // Building gene feature data item; 
  GeneFeatureItem* pFeatureItem = new GeneFeatureItem(f_dat, g_c); 
  if(!pFeatureItem) {
    cerr << "Can't create new GeneFeatureItem object. " << endl; 
    return false; 
  }
  //cout << "Inserting ... ... " << pFeatureItem->getFeature() << endl;
  f_data.push_back(*pFeatureItem); 

  // refresh the high and low boundary of the gene data sample set. 
  if(f_dat > f_highest)
    f_highest = f_dat; 
  if(f_dat < f_lowest) 
    f_lowest = f_dat; 
  return true; 
}

// Print the content of this feature data. 
void GeneFeatureData::print() {
  vector<GeneFeatureItem>::iterator it = f_data.begin(); 
  while(it != f_data.end()) {
    it->print(); 		// print the data for feature item. 
    it++;
  }
}

// Do equi-density binning. 
bool GeneFeatureData::equiDensityBinning(int num_bins) {
  float interval = (f_highest - f_lowest) / num_bins; 
  // Creating bins. 
  for(int i = 0; i < num_bins; i++) {
    GeneFeatureBins* pBin = new GeneFeatureBins(f_lowest+i*interval, 
						f_lowest+(i+1)*interval);
    if(!pBin) {cerr << "Error creating bins..." << endl; return false;}
    f_density_bins.push_back(*pBin); 
  } // for 

  // Inserting data into bins. 
  for(int i = 0; i < num_bins; i++){
    //GeneFeatureBins* pBin = ; 
    gene_feature_t high = f_density_bins.at(i).getHighBoundary();
    gene_feature_t low = f_density_bins.at(i).getLowBoundary();
    for(int j = 0; j < f_data.size(); j++) {
      GeneFeatureItem f_item = f_data.at(j); 
      // cout << "low: " << low << " high: " << high << " item: "
      // 	   << f_item.getFeature() << endl; 
      if((f_item.getFeature() >= low) && (f_item.getFeature() < high)){
	//cout << "bingo...." << endl; 
	f_density_bins.at(i).insertItem(f_item);
      }
      //cout << f_item.getFeature() << endl; 
    } //for data
  } // for bin
  // test printing equi-density bins. 
  printEDBins(); 
}

// Print equi-density bins. 
void GeneFeatureData::printEDBins() {
  cout << "Contents of bin: " << endl; 
  vector<GeneFeatureBins>::iterator it = f_density_bins.begin(); 
  while(it != f_density_bins.end()) {
    it->print(); 
    it++;
  }
}

// Do entropy based binning. 
bool GeneFeatureData::entropyDiscretize() {

}
