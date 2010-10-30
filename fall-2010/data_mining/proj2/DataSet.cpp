#include "DataSet.h"
#include <cstdio> 		/* for sprintf() */

// Load gene data from file.  
bool DataSet::loadFromFile(string fname) {
  fstream fdata; 
  fdata.open(fname.c_str(),fstream::in);
  if(!fdata) return false; 

  string line; 	// tmp str and num k.
  gene_class_t d_class; 	// class of gene.
  DataSet* pDataSet = NULL; // pointer to gene data set. 
  int row = -1, col = -1; 
  int totalrow, totalcol;
  cout << "Start loading data ... " << endl; 
  while(fdata) {
    if(!fdata.good()) {
      cerr<<"Error while reading data file."<<endl; return 1;
    }
    getline(fdata, line, fdata.widen('\n'));
    if(line == "") break;
    row++;			// row count.
#ifdef DEBUG_DATA_LOAD
    cout << "process row: " << row << endl;
#endif
    vector<Item> td;
    // Parsing row data.
    while(1) {
      int poscomma = line.find(',',0);
      if(poscomma != string::npos) { // column by column process.
	string tmp=line.substr(0, poscomma);
	col++;
	td.push_back(Item(tmp[0])); 
	line=line.substr(poscomma+1,line.size()-poscomma).c_str();
      } else {			// End of line.
	col++;
	if(row >= 0 && col >= 0) {
	  td.push_back(Item(line[0]));
	  d_sets.push_back(td);
	  totalrow = row; totalcol = col;
#ifdef DEBUG_DATA_LOAD
	  cout << "Saveing row " << row << " data.." << endl;
	  cout << "load " << col 
	       << " cols. " 
	       << " line number: " << row << endl;
#endif
	}
	col = -1; break;
      }	// while line.
    }
  } //while(fdata)
  fdata.close();
  setNumGenes(totalcol + 1);
  setNumTrans(totalrow + 1);
  // Statistics. 
  cout << "Finished loading data. " << endl
       << "Totally loaded " << getNumTrans()
       << " lines and " << getNumGenes()
       << " columns of data." << endl;

  cout << "Load successful ..." << endl; 
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
  int count; 
  map<char, int> mp;		// 
  for(int i = 0; i < getNumGenes(); i++){
    int cnt = 0; mp.clear();
    for(int j = 0; j < getNumTrans(); j++){
      char grp = d_sets[j][i].getGroup();
      if(!mp.count(grp)) {
	mp[grp] = cnt;
	d_sets[j][i].setId(count + mp[grp]);
	cnt++;
      } else {
	d_sets[j][i].setId(count + mp[grp]);
      }	// if else
      // collect first level frequent sets.
      scanLevelOne(count + mp[grp]);
#ifdef DEBUG_ITEM_MAP
      cout << " group: " << grp << " value in gene: " << mp[grp] 
	   << " uniq cnt: " << cnt 
	   << " id: " << count + mp[grp] << endl;
#endif
    } // for rows.
    count += cnt;
  }// for cols.
}

// Scan level one frequent sets.
void DataSet::scanLevelOne(int id) {
  if(!fst[id]) {
    fst[id] = 1;
  } else {
    fst[id]++;
  }
}

// Scan level two item sets. 
void DataSet::scanLevelTwo(){
  // for Item in col i and col j > i
  // if they are joinable, then, join them and 
  for(int i = 0; i < getNumTrans(); i++){
    for(int j = 0; j < getNumGenes(); j++){ // col j; 
      for(int k = j + 1; k < getNumGenes(); k++){ // col k > j;	
	//string key = d_sets[i][j] 
	Itemset set1, set2; // do joining.
	set1.pushBack(d_sets[i][j]);
	set2.pushBack(d_sets[i][k]);
	set1 = set1.join(set2);
	if(snd.find(set1) == snd.end()) {
	  snd[set1] = 1;
	} else {
	  snd[set1]++;
	}
	cout << " key: " << set1
	     << " count: " << snd[set1]
	     << endl;
      }
    }
  }
}

// Print level one item set frequencies. 
void DataSet::printLevelOne(){
  map<int, int>::iterator it; 
  for(it = fst.begin(); it != fst.end(); it++) {
    cout << (*it).first << ":" << (*it).second << endl;
  }
}

// Print level two item set frequencies. 
void DataSet::printLevelTwo(){
  map<Itemset, int>::iterator it; 
  for(it = snd.begin(); it != snd.end(); it++) {
    cout << (*it).first << ":" << (*it).second << endl;
  }
}
// Save item map into given file. 
bool DataSet::saveItemMap(string fname) {
  ofstream fdata(fname.c_str());
  // Check if open file successful.
  if(!fdata) {
   cerr << "Error opening file to save map data" << endl; 
   return false;
  }
  for(int i = 0; i < getNumTrans(); i++){
    for(int j = 0; j < getNumGenes(); j++){
      fdata << d_sets[i][j] << ",";    
    }
    fdata << endl; 
  }
  fdata.close();
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
  char grp = str[0];
  Item item(grp);

  // Then, insert the data item into the vector. 
  // Store column-wise. 
  // if(getNumGenes() < col_num+1) {
  //   GeneData gset();
  //   addGeneDataObj(gset);
  // }
  // d_sets.at(col_num).push_back(item);

  // // Store row-wise.
  // if(getNumTrans() < row_num+1) {
  //   TransData tset();
  //   addTransDataObj(tset);
  // }
  // t_sets.at(row_num).push_back(item);
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

string itoa(const int &integer){
  if (integer==0) return string("0");
  string a;
  int start, digits, piece;

  //count digits
  digits=0;
  piece=((integer<0)? 0-integer : integer);
  while( piece > 0 ) {
    piece-= (piece%10);
    piece/=10;
    digits++;
  }
  
  start=((integer<0)? 1 : 0);
  a.resize(digits+start,' ');
  if (integer<0) a[0]='-';
  
  piece=((integer<0)? 0-integer : integer);
  for(int i=0;  piece > 0; i++ ) {
    a[ digits+start-i-1] = (piece%10)+48;
    piece-= (piece%10);
    piece/=10;
  } 
  return a;
}
