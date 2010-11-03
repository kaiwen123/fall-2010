#include "DataSet.h"

using namespace std; 

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
  pDataSet->setMinSupport(atoi(argv[2]));
  pDataSet->setMinConf(atoi(argv[3]));
  string fname; 
  // p2entbindata.txt | p2eqbindata.txt
  fname = string("p2entbindata.txt"); 
  fname = string("5d1"); 
  //fname = string("5d");
  pDataSet->loadFromFile(fname);

  // Do Item mapping, which will map the gene data into unique ids for
  // different genes. And the mapping information will be saved into 
  // the designated file. 
  cout << "Doing Item mapping.....";
  pDataSet->doItemMap();

  fname = string("p2ItemMap.txt"); 
  pDataSet->saveItemMap(fname); 

  cout << "done!" << endl;

  // Now start the APRIORI process. 
  // We need to do level one and level two APRIORI mining, and then
  // using the hash tree to do APRIORI mining of other levels. 
  // pDataSet->printLevelFreqSets(1);
  // pDataSet->scanLevelTwo();
  // pDataSet->printLevelFreqSets(2);
  pDataSet->doApriori(); 

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
       << "           Should be within (1,MAX).\n" 
       << "minConf  - The minimum confidence for assoc rule. \n" 
       << "           Should be within (0.1.0]\n" 
       << "g        - The number of genes to process. \n" 
       << "k        - The number of rules to be printed. " 
       << endl;
}
