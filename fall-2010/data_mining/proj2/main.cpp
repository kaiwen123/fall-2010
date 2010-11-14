#include "Itemset.h"
#include "DataSet.h"

using namespace std; 

// Global variables.
int numTrans = 0, g = 0, k = 0;
float minSupport = 0.0;; 
float minConf = 0.0;

void Usage();

int main(int argc, char *argv[]) {
  if(argc != 6 || atoi(argv[3]) < 0 || atoi(argv[3]) > 1.0) {
    Usage();
    return 1;
  }
  // Constructing data sets. 
  DataSet *pDataSet = new (std::nothrow) DataSet();
  if(!pDataSet) {
    cerr << "Can't create the GeneDataSet object." << endl;
    return 0;
  }
  minSupport = atof(argv[2]);
  if(minSupport > 1 || minSupport <= 0) {
    cerr << "Invalide minimum support." << endl; return 1; 
  } 
  if(minSupport < 0.3) {
    cerr << "WARNING!!! Minimum support TOO SMALL...!" << endl;
  }
  minConf = atof(argv[3]);
  if(minConf > 1 || minConf <= 0) {
    cerr << "Invalide minimum confidence." << endl; return 1; 
  }
  g = atoi(argv[4]);
  k = atoi(argv[5]);

  // Load data from file.
  string fname; 
  fname = string(argv[1]);
  pDataSet->loadFromFile(fname);

  // Do Item mapping, which will map the gene data into unique ids for
  // different genes. And the mapping information will be saved into 
  // the designated file. 
  cout << "Doing Item mapping"; flush(cout); 
  pDataSet->doItemMap();
  cout << " ...... done!" << endl;flush(cout); 

  fname = string("p2ItemMap.txt"); 
  cout << "Save item map to " << fname;flush(cout); 
  pDataSet->saveItemMap(fname); 
  cout << " ...... done!" << endl;flush(cout); 

  // Now start the APRIORI Algorithm. 
  // We need to do level one and level two APRIORI mining, and then
  // using the hash tree to do APRIORI mining of other levels. 
  
  cout << "Doing Apriori Algorithm on data set "; flush(cout); 
  pDataSet->doApriori(); 
  cout << " ...... done!" << endl; flush(cout); 

  // Save Freqient item sets into file. 
  fname = string("p2FreqItemsets.txt"); 
  cout << "Saving frequent itemsets to " << fname; flush(cout); 
  pDataSet->saveFreqItemSets(fname);
  cout << " ...... done!" << endl;flush(cout); 

  cout << "Generating Association rules "; flush(cout); 
  pDataSet->genAssoRule();
  cout << " ...... done!" << endl; flush(cout); 

  cout << "\nTop " << k 
       << " association rules ranked by sup*conf:\n\n"; flush(cout); 
  pDataSet->printAssoRule();
  cout << "done!" << endl;
  
  delete pDataSet;
  return 0; 
}

// Auxilliary functions .
void Usage() {
  cout << "Usage: "
       << "./AssoRuleMiner datafile minSup minConf g k"
       << "\n"
       << "datafile - The file that contains the gene data.\n" 
       << "minSup   - The minimum support for association rule.\n"
       << "           Should be within (1,MAX).\n" 
       << "minConf  - The minimum confidence for assoc rule. \n" 
       << "           Should be within (0.1.0]\n" 
       << "g        - The number of genes to process. \n" 
       << "k        - The number of rules to be printed. " 
       << endl;
}
