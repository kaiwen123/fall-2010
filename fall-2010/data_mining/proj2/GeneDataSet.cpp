#include "GeneDataSet.h"
#include <cstdio> 		/* for sprintf() */
#include <algorithm>		// for sort algorithm

// Default constructor. 
GeneDataSet::GeneDataSet() {
}

// Constructor with number of features. 
GeneDataSet::GeneDataSet(int num_f) {
  for(int i = 0; i < num_f; i++) {
    GeneFeatureData f_data; 
    f_sets.push_back(f_data); 
  } // for loop
}

// Create gene data. 
bool GeneDataSet::createGeneData(int num_gene) {
  for(int i = 0; i < num_gene; i++) {
    GeneFeatureData f_data; 
    f_sets.push_back(f_data); 
  } // for loop
}

// Load gene data from file.  
bool GeneDataSet::loadFromFile() {
  string fname; fstream fdata; 
  do {
    cout << "Please enter the name of data file: ";
    cin >> fname; 
    fdata.open(fname.c_str(),fstream::in);
    if(fdata) break; 
  } while(1);

  string x, line; int k;	// tmp str and num k.
  vector<string> g_data; 	// data for a gene. 
  gene_class_t d_class; 	// class of gene.
  GeneDataSet* pGeneSet = NULL; // pointer to gene data set. 
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
	string tmp=line.substr(0, poscomma);
	g_data.push_back(tmp);
	line=line.substr(poscomma+1,line.size()-poscomma).c_str();
	//cout << d_tmp << " " << line << endl; 
      } else break;
    }
    //if(!pGeneSet) pGeneSet = new GeneDataSet(g_data.size()); 
    createGeneData(g_data.size());
    for(int i = 0; i < g_data.size(); i++)
      //insertData(i, g_data.at(i), d_class); 
    g_data.clear();
  } //while(fdata)
  fdata.close();
  if(!pGeneSet) return 1;
  int num_tissues = getNumRows();
  int num_genes = getNumFeatures();
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
}

// Map gene data into unique identifiers. 
bool GeneDataSet::doItemMap() {
  int counter = 1; 
  vector<GeneFeatureData>::iterator _itgenedata = f_sets.begin();
  // while(_itgenedata != f_sets.end()){
  //   vector<GeneFeatureItem>::iterator _itt = (_itgenedata->getFData()).begin();
  //   while(_itt != _itgenedata->begin()){
  //     int id = counter + _itt->getFeature() - 'a';
  //     cout << "id :" << id << __LINE__ << endl;
  //     _itt++;
  //   } // gene data item. 
  //   _itgenedata++;
  // } // item sets. 
}

// Save item map into given file. 
bool GeneDataSet::saveItemMap(string fname) {

}

// Do apriori association rule mining to the gene set. 
bool GeneDataSet::doApriori() {

}

// Save frequent itemsets into given file.
bool GeneDataSet::saveFreqItemSets(string fname) {

}
// Insert data into the data set. 
bool GeneDataSet::insertData(int f_id, 
			     gene_feature_t f_data, 
			     gene_class_t g_class) {
  char buf[6]; sprintf(buf, "%d", f_id);
  if(f_sets.at(f_id).getFid() == "")
    f_sets.at(f_id).setFid("G"+string(buf));
  //f_sets.at(f_id).insert(f_data, g_class); 
  return true; 
} 

// Overload << operator for gene_class_t. 
ostream& operator<<(std::ostream& out, const gene_class_t& c) {
  switch(c) {
  case positive: out << "positive"; break; 
  case negative: out << "negative"; break; 
  }
  return out;
}

// Overloading operator <<. 
// This operator will output everything to output stream. 
ostream& operator<<(ostream& out, GeneDataSet& g) {
  int col_count = g.f_sets.size();
  for(int i = 0; i < g.getNumRows(); i++) {
    gene_class_t c; 
    for(int j=0; j<col_count-1; j++){
      c = g.f_sets.at(j).getFData().at(i).getClass();
      out << g.f_sets.at(j).getFData().at(i);
    }
    out << c << endl;
  }
  return out;
}
