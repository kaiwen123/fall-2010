#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <stdlib.h>
#include "defs.h"
#include "GeneDataSet.h"

using namespace std; 

// The main() function. 
// It will load in all the data from data file. 
// and store all the data to GeneDataSet Object, the data set object
// uses vector to store column(gene) data. 
// After loading in all the data, it will call the binning methods of
// data set to do equi-width and entropy based binning. 
// And finally, it will call methods of data set to save the binning
// result and the bins to files according to the project requirement.
int main() {
  // Constructing data sets. 
  string fname; fstream fdata; 
  do {
    cout << "Please enter the name of data file: ";
    cin >> fname; 
    fdata.open(fname.c_str(),fstream::in);
    if(fdata) break; 
  } while(1);

  string x; int k;		// tmp str and num k.
  vector<double> g_data; 	// data for a gene. 
  gene_feature_t d_tmp; 	// temporary gene data. 
  gene_class_t d_class; 	// class of gene.
  GeneDataSet* pGeneSet = NULL; // pointer to gene data set. 
  char** p; 			// used for operation strtod.
  cout << "Start loading data ... " << endl; 
  while(fdata) {
    if(!fdata.good()) {
      cerr<<"Error while reading data file."<<endl; return 1;
    }
    fdata >> x; 
    // For feature data, strip the ',' and transform to double. 
    if(x.find(',',0)!=string::npos) {
      d_tmp = strtod(x.substr(0, x.find(',',0)).c_str(), p);
      g_data.push_back(d_tmp);
      //cout << d_tmp << endl; 
    }
    // The end of line is reached. 
    // find data class, positive or negative. 
    if(x.find('p',0)!=string::npos || x.find('n',0)!=string::npos){
      if(x.find('p',0)!=string::npos) d_class = positive; 
      else d_class = negative; 
      // Create data set object if it doesn't exist yet.
      if(!pGeneSet) pGeneSet = new GeneDataSet(g_data.size()); 
      for(int i = 0; i < g_data.size(); i++)
	pGeneSet->insertData(i, g_data.at(i), d_class); 
      g_data.clear(); fdata >> x; // to avoid errors. 
    } //if
  } //while(fdata)
  fdata.close();
  if(!pGeneSet) return 1;
  int num_tissues = pGeneSet->getNumRows();
  int num_genes = pGeneSet->getNumFeatures();
  // Statistics. 
  cout << "Finished loading data. " << endl
       << "Totally loaded " << num_tissues
       << " lines and " << num_genes
       << " rows of data." << endl;
  
  cout << "Please input number of genes(k) to process: ";
  while(!(cin >> k)||(cin.get() != '\n')
	&&!(k>0)&&!(k<=num_genes)) {
    cout << "please enter an integer within range: ["
	 << "0, " << num_genes << "]: ";
  }

  // First, we need to do binning for each data set for the gene data
  // include equi-width binning and entropy binning. 
  for(int i = 0; i < num_genes; i++) {
    pGeneSet->doEquiWidthBin(i, 4);
    pGeneSet->doEntropyDiscretize(i, 2);
  }
  // Save equi-width data. 
  fname = string("ewidth.data");
  pGeneSet->saveEquiWidthData(fname, k); // TODO

  // Save Equi-width bins. 
  fname = string("ewidth.bins");
  pGeneSet->saveEquiWidthBins(fname, k); // finished

  // Save entropy data. 
  fname = string("entropy.data"); 
  pGeneSet->saveEntropyData(fname, k);

  // Save entropy bins. 
  fname = string("entropy.bins"); 
  pGeneSet->saveEntropyBins(fname, k);


  // save everything into file. 
  // string fsname("ewidth.data"); 
  // ofstream saveFile(fsname.c_str());
  // saveFile << *pGeneSet; 
  // saveFile.close();

  // Output data into files. 
  // pGeneSet->saveEquiWidthData(); 
  
  // pGeneSet->saveEntropyData(); 
  // pGeneSet->saveEntropyBins();
  // pGeneSet->findTopkGene(1);
  return 0; 
}
