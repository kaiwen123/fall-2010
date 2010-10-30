#include "DataStore.h"

GeneData::GeneData() {
  fid = "";
  p_count = 0;
  n_count = 0;
}

// Inserting data into the gene feature data. 
// bool GeneData::insert(gene_feature_t f_dat, 
// 			     gene_class_t g_c) {
//   // Building gene feature data item and insert into vector. 
//   Item* pF = new Item(f_dat, g_c); 
//   if(!pF){cerr<<"Can't create new Item object."<<endl;return false;}
//   f_data.push_back(*pF); 
//   // refresh the high and low boundary of the gene data sample set. 
//   if(f_dat>f_highest) f_highest=f_dat; 
//   if(f_dat<f_lowest) f_lowest=f_dat;

//   // update the positive and negative data counters.
//   if(g_c == positive) p_count++; 
//   else n_count++;
//   return true; 
// }


// Overloading << operator. 
ostream& operator<<(ostream& out, GeneData& d) {
  vector<Item>::iterator _dit = d.getFData().begin(); 
  while(_dit != d.getFData().end()) {
    out << *_dit; 
    _dit++;
  }
  return out; 
}

// overloading the >= operator. 
// Compare two objects according to info-gain.
// bool GeneData::operator>(GeneData& d) {
//   return getInfoGain() > d.getInfoGain();
// }
