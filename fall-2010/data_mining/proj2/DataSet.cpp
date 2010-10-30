#include "DataSet.h"
#include <cstdio> 		/* for sprintf() */
#include <algorithm>		// for sort algorithm

// Default constructor. 
DataSet::DataSet() {}

// Constructor with number of features. 
DataSet::DataSet(int num_f) {
}

// Load gene data from file.  
bool DataSet::loadFromFile(string fname) {
  fstream fdata; 
  fdata.open(fname.c_str(),fstream::in);
  if(!fdata) return false; 

  string line; 	// tmp str and num k.
  gene_class_t d_class; 	// class of gene.
  DataSet* pDataSet = NULL; // pointer to gene data set. 
  int row = -1, col = -1; 
  cout << "Start loading data ... " << endl; 
  while(fdata) {
    if(!fdata.good()) {
      cerr<<"Error while reading data file."<<endl; return 1;
    }
    getline(fdata, line, fdata.widen('\n'));
    row++;			// row count.
    
    while(1) {
      int poscomma = line.find(',',0);
      if(poscomma!=string::npos) {
	string tmp=line.substr(0, poscomma);
	col++;
	insertData(row, col, tmp); // Insert into dataset. 
	line=line.substr(poscomma+1,line.size()-poscomma).c_str();
      } else {
	col++;
	if(row > 0 && col > 0) {
	  insertData(row, col, line); 
#ifdef DEBUG_DATA_LOAD
	  cout << "load " << col 
	       << " cols. " 
	       << " line number: " << row << endl;
#endif
	}
	col = -1;
	break;
      }
    }
  } //while(fdata)
  fdata.close();
  int num_tissues = getNumTrans();
  int num_genes = getNumFeatures();

  // Statistics. 
  cout << "Finished loading data. " << endl
       << "Totally loaded " << row
       << " lines and " << col
       << " columns of data." << endl;
  
  // cout << "Please input number of genes(k) to process: ";
  // while(!(cin >> k)||(cin.get() != '\n')
  // 	||!(k>=0 && k<=num_genes)) {
  //   cin.clear(); cin >> noskipws;
  //   cin.ignore(1000, '\n');
  //   cout << "please enter an integer within range: ["
  // 	 << "0, " << num_genes << "]: ";
  // }
}

// Map gene data into unique identifiers. 
bool DataSet::doItemMap() {
  // int counter = 1; 
  // vector<GeneData>::iterator _itgenedata = f_sets.begin();
  // while(_itgenedata != f_sets.end()){
  //   vector<Item>::iterator _itt = (_itgenedata->getFData()).begin();
  //   while(_itt != _itgenedata->begin()){
  //     int id = counter + _itt->getFeature() - 'a';
  //     cout << "id :" << id << __LINE__ << endl;
  //     _itt++;
  //   } // gene data item. 
  //   _itgenedata++;
  // } // item sets. 
}

// Save item map into given file. 
bool DataSet::saveItemMap(string fname) {

}

// Do apriori association rule mining to the gene set. 
bool DataSet::doApriori() {

}

// Save frequent itemsets into given file.
bool DataSet::saveFreqItemSets(string fname) {

}
// Insert data into the data set. 
bool DataSet::insertData(int row_num, int col_num, string str) {
#ifdef DEBUG_DATA_LOAD
  cout << "row: " << row_num
       << " col: " << col_num
       << " content: " << str << endl;
#endif
  // First, create data item. 

  // Then, insert the data item into the vector. 

//   char buf[6]; sprintf(buf, "%d", f_id);
//   if(f_sets.at(f_id).getFid() == "")
//     f_sets.at(f_id).setFid("G"+string(buf));
//   //f_sets.at(f_id).insert(f_data, g_class); 
  return true; 
} 

// Overload << operator for gene_class_t. 
// ostream& operator<<(std::ostream& out, const gene_class_t& c) {
//   switch(c) {
//   case positive: out << "positive"; break; 
//   case negative: out << "negative"; break; 
//   }
//   return out;
// }

// Overloading operator <<. 
// This operator will output everything to output stream. 
// ostream& operator<<(ostream& out, DataSet& g) {
//   int col_count = g.f_sets.size();
//   for(int i = 0; i < g.getNumRows(); i++) {
//     gene_class_t c; 
//     for(int j=0; j<col_count-1; j++){
//       c = g.f_sets.at(j).getFData().at(i).getClass();
//       out << g.f_sets.at(j).getFData().at(i);
//     }
//     out << c << endl;
//   }
//   return out;
// }

#define HASHSIZE 17

int hashfunc(int value) {
  return value % HASHSIZE; 
}
