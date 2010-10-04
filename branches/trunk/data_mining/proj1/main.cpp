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

  string x, line; int k;	// tmp str and num k.
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
    getline(fdata, line, fdata.widen('\n'));
    
    // Positive or negative class.
    if(line.find('p',0)!=string::npos) d_class = positive; 
    else d_class = negative;
      
    //cout << line << endl; 
    while(1) {
      int poscomma = line.find(',',0);
      if(poscomma!=string::npos) {
	d_tmp=strtod(line.substr(0, poscomma).c_str(), p);
	g_data.push_back(d_tmp);
	line=line.substr(poscomma+1,line.size()-poscomma).c_str(); 
	//cout << d_tmp << " " << line << endl; 
      } else break;
    }
    if(!pGeneSet) pGeneSet = new GeneDataSet(g_data.size()); 
    for(int i = 0; i < g_data.size(); i++)
      pGeneSet->insertData(i, g_data.at(i), d_class); 
    g_data.clear();
  } //while(fdata)
  fdata.close();
  if(!pGeneSet) return 1;
  int num_tissues = pGeneSet->getNumRows();
  int num_genes = pGeneSet->getNumFeatures();
  // Statistics. 
  cout << "Finished loading data. " << endl
       << "Totally loaded " << num_tissues
       << " lines and " << num_genes
       << " columns of data." << endl;
  
  cout << "Please input number of genes(k) to process: ";
  while(!(cin >> k)||(cin.get() != '\n')
	||!(k>=0 && k<=num_genes)) {
    cin.clear(); cin >> noskipws;
    cin.ignore(1000, '\n');
    cout << "please enter an integer within range: ["
	 << "0, " << num_genes << "]: ";
  }

  // Do equi-width binning. 
  for(int i = 0; i < num_genes; i++)
    pGeneSet->doEquiWidthBin(i, 4);
    
  // Save equi-width data. 
  fname = string("ewidth.data");
  pGeneSet->saveEquiWidthData(fname, k); // TODO

  // Save Equi-width bins. 
  fname = string("ewidth.bins");
  pGeneSet->saveEquiWidthBins(fname, k); // finished

  // Do Entropy Discretization. 
  for(int i = 0; i < num_genes; i++) 
    pGeneSet->doEntropyDiscretize(i, 2);

  // Save entropy data. 
  fname = string("entropy.data"); 
  pGeneSet->saveEntropyData(fname, k);

  // Save entropy bins. 
  fname = string("entropy.bins"); 
  pGeneSet->saveEntropyBins(fname, k);

  // print info gain.
#ifdef PRINT_GAIN_INFO
  pGeneSet->printInfoGain();
#endif
  return 0; 
}
