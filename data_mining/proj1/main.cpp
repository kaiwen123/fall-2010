#include <iostream>
#include <fstream>
#include <string>
#include <vector>
//#include <cstdlib>
#include <stdlib.h>
#include "defs.h"
#include "GeneDataSet.h"

using namespace std; 

int main() {
  // Constructing data sets. 
  fstream fdata;
  const char* fname = "p1data.txt";
  fdata.open(fname, fstream::in);
  string x; 
  vector<double> g_data; 	// data for a gene. 
  gene_feature_t d_tmp; 	// temporary gene data. 
  gene_class_t d_class; 	// class of gene.
  int num_f;			// total number of features. 
  GeneDataSet* pGeneSet = NULL; // pointer to gene data set. 
  char** p; 

  while(fdata) {
   
    if(!fdata.good()) {cerr<<"Error while reading file. "<<endl;}
    fdata >> x; 

    // For feature data, strip the ',' and transform to double. 
    if(x.find(',',0)!=string::npos) {
      d_tmp = strtod(x.substr(0, x.find(',',0)).c_str(), p);
      g_data.push_back(d_tmp);
    }

    // The end of line is reached. 
    // find data class, positive or negative. 
    if(x.find('p',0)!=string::npos || x.find('n',0)!=string::npos){
      if(x.find('p',0)!=string::npos) 
	d_class = positive; 
      else 
	d_class = negative; 

      // Put data into gene feature data object.
      if(!pGeneSet) {
	pGeneSet = new GeneDataSet(g_data.size()); 
      }
      for(int i = 0; i < g_data.size(); i++) {
	//cout << g_data.at(i) << " " << g_data.size() << endl; 
	pGeneSet->insertData(i, g_data.at(i), d_class); 
      } // for     
      g_data.clear(); 
    } //if
  } //while(fdata)
  // Test by printing out the result. 
  // for(int i = 0; i < pGeneSet->getNumFeatures(); i++) {
  //   pGeneSet->print(i); 
  // }
  pGeneSet->doEquiDensityBin(0, 4); 

  fdata.close(); 
  // Equidensity binning. 

  // Entropy Based binning. 

  return 0; 
}
