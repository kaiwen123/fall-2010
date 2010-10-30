#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <stdlib.h>
#include "defs.h"
#include "DataSet.h"

using namespace std; 
void Usage();
// The main() function. 
// It will load in all the data from data file. 
// and store all the data to GeneDataSet Object, the data set object
// uses vector to store column(gene) data. 
// After loading in all the data, it will call the binning methods of
// data set to do equi-width and entropy based binning. 
// And finally, it will call methods of data set to save the binning
// result and the bins to files according to the project requirement.
int main(int argc, char *argv[]) {
  if(argc != 5) {
    Usage();
    return 1;
  }
  // Constructing data sets. 
  DataSet *pDataSet = new (std::nothrow) DataSet();
  if(!pDataSet) {
    cerr << "Can't create the GeneDataSet object." << endl;
    return 0;
  }
  string fname; 
  fname = string("p2entbindata.txt"); // p2entbindata.txt | p2eqbindata.txt
  pDataSet->loadFromFile(fname);

  // Do Item mapping, which will map the gene data into uniq ids for
  // different genes. 
  //pDataSet->doItemMap();

  // Save the mapped unique gene data into file. 
  //fname = string("p2ItemMap.txt"); 
  //pDataSet->saveItemMap(fname); 

  // Do the APRIORI association rule mining. 
  //pDataSet->doApriori(); 

  // Save Freqient item sets into file. 
  //fname = string("p2FreqItemsets.txt"); 
  //pDataSet->saveFreqItemSets(fname);
  delete pDataSet;

  return 0; 
}

void Usage() {
  cout << "Usage: "
       << "./AssoRuleMiner datafile minSup minConf g k"
       << "\n"
       << "datafile - The file that contains the gene data.\n" 
       << "minSup   - The minimum support for association rule.\n"
       << "minConf  - The minimum confidence for assoc rule. \n" 
       << "g        - The number of genes to process. \n" 
       << "k        - The number of rules to be printed. " 
       << endl;
}
