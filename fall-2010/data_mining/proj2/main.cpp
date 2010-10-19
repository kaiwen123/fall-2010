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
int main(int argc, char *argv[]) {
  // Constructing data sets. 
  GeneDataSet *pGeneSet = new (std::nothrow) GeneDataSet();
  if(!pGeneSet) {
    cerr << "Can't create the GeneDataSet object." << endl;
    return 0;
  }
  string fname; 
  fname = string("datafile");
  pGeneSet->loadFromFile();

  // Do Item mapping, which will map the gene data into uniq ids for
  // different genes. 
  pGeneSet->doItemMap();

  // Save the mapped unique gene data into file. 
  fname = string("p2ItemMap.txt"); 
  pGeneSet->saveItemMap(fname); 

  // Do the APRIORI association rule mining. 
  pGeneSet->doApriori(); 

  // Save Freqient item sets into file. 
  fname = string("p2FreqItemsets.txt"); 
  pGeneSet->saveFreqItemSets(fname);

  return 0; 
}
