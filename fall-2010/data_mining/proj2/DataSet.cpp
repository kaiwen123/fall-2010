#include "DataSet.h"

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
  cout << "Start loading data ...... "; 
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
	td.push_back(Item(tmp[0], col)); 
	line=line.substr(poscomma+1,line.size()-poscomma).c_str();
	// Control number of genes to process. 
	if(col == g - 2) {	// g-1 cols done. 
	  //cout << "col = " << col << " g = " << g << " " << line << endl;
	  if(line.find('p',0) != string::npos) {
	    col++; Item item('p', col);
	    td.push_back(item); 
	  }
	  if(line.find('n',0) != string::npos) {
	    col++;Item item('n', col);
	    td.push_back(item); 
	  }
	  d_sets.push_back(td);
	  totalrow = row; totalcol = col;
	  col = -1;
	  break;
	}
      } else {			// End of line.
	col++;
	if(row >= 0 && col >= 0) {
	  td.push_back(Item(line[0], col));
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
  numTrans = totalrow + 1; 
  if(g > totalcol + 1) g = totalcol + 1;

  // Statistics. 
  cout << " done! " << endl
       << "Totally loaded " << numTrans
       << " lines and " << g
       << " columns of data." << endl;
}

// Map gene data into unique identifiers. 
bool DataSet::doItemMap() {
  int count = 0; 		// Initialize it!!
  map<char, int> mp;
  for(int i = 0; i < g; i++){
    int cnt = 0; mp.clear();
    for(int j = 0; j < numTrans; j++){
      char grp = d_sets[j][i].getGroup();
      if(!mp.count(grp)) {
	mp[grp] = cnt;
	d_sets[j][i].setId(count + mp[grp]);
	cnt++;
      } else {
	d_sets[j][i].setId(count + mp[grp]);
      }	// if else
      // collect first level frequent sets.
      Itemset set; set.pushBack(d_sets[j][i]);
      scanLevelOne(set);
#ifdef DEBUG_ITEM_MAP
      cout << " group: " << grp << " value in gene: " << mp[grp] 
	   << " uniq cnt: " << cnt 
	   << " id: " << count + mp[grp] << endl;
#endif
    } // for rows.
    count += cnt;
  }// for cols.
}

// // Scan level one frequent sets.
void DataSet::scanLevelOne(Itemset set) {
  if(freqsets.find(set) == freqsets.end()) {
    freqsets[set] = 1;
  } else {
    freqsets[set]++;
  }
}

// Scan level two item sets. 
void DataSet::scanLevelTwo(){
  // A new version. 
  map<Itemset, int>::iterator itset; 
  for(itset = freqsets.begin(); itset != freqsets.end(); itset++) {
    float spt = (float)(*itset).second/numTrans;
    if(spt < minSupport) {
      freqsets.erase(itset->first);
    }
  }

  // Now, scan and get level two frequent itemsets. 
  map<Itemset, int>::iterator itfst; 
  for(itfst = freqsets.begin(); itfst != freqsets.end(); itfst++) {
    map<Itemset, int>::iterator itsnd; 
    for(itsnd = itfst; itsnd != freqsets.end(); itsnd++) {
      Itemset set1 = itfst->first;
      Itemset set2 = itsnd->first;
      if(!(set1 == set2) 
	 && (set1.getSize() == 1)
	 && (set2.getSize() == 1)) {
	Itemset set = set1 + set2; 
	int count = scanItemset(set);
	float sup = (float)count/numTrans;
	if(sup >= minSupport) {
	  freqsets[set] = count;
	} // pruning check. 
      }	// if combine two level one sets into one level two sets.
    } // for in loop
  } // for out loop
}

// scan itemset and get the number of occurances. 
int DataSet::scanItemset(Itemset& set) {
  int count = 0;
  bool equal = true;
  for(int i = 0; i < numTrans; i++){
    equal = true;
    for(int j = 0; j < set.getSize(); j++) {
      int colpos = set[j].getColumnPos();
#ifdef DEBUG_APRIORI_SCAN
      cout << "Scan item: " << set[j] << " vs. " << d_sets[i][colpos] 
	   << " col: " << colpos << " ";
#endif
      if(set[j] != d_sets[i][colpos]) {
	equal = false; break; 
      }
    }
#ifdef DEBUG_APRIORI_SCAN
    cout << endl;
#endif
    if(equal) count++;
  }
#ifdef DEBUG_APRIORI_SCAN
  cout << "Itemset scan:" << set << ":" << count << endl;
#endif
  return count;
}

// Save item map into given file. 
bool DataSet::saveItemMap(string fname) {
  ofstream fdata(fname.c_str());
  if(!fdata) {
   cerr << "Error opening file to save map data" << endl; 
   return false;
  }
  for(int i = 0; i < numTrans; i++){
    for(int j = 0; j < g; j++){
      fdata << d_sets[i][j].getGroup() << " " 
	    << d_sets[i][j].getId();
      if(j < g-1) {
	fdata << ",";
      }
    }
    fdata << endl; 
  }
  fdata.close();
}

// Do apriori association rule mining to the gene set. 
// nindex is used to store the index of all the nodes of 
// the hash tree.
bool DataSet::doApriori() {
  // Scan level one and joined it to level two. 
  scanLevelTwo();
  // Now, let's do the APRIORI algorithm.
  map<Itemset, int>::iterator it1; 
  for(it1 = freqsets.begin(); it1 != freqsets.end(); it1++) {
    map<Itemset, int>::iterator it2; 
    for(it2 = it1; it2 != freqsets.end(); it2++) {
      Itemset set1 = it1->first; 
      Itemset set2 = it2->first;
      bool canJoin = set1.isJoinable(set2);       
      if(canJoin) {
	Itemset set = set1.join(set2);
	int count = scanItemset(set); 
	float spt = (float)count / numTrans; 
	if(freqsets.find(set) == freqsets.end() 
	   && spt >= minSupport) {
#ifdef DEBUG_APRIORI
	  cout << set1 << ":" << (float)freqsets[set1]/numTrans
	       << " " << set2 << ":" << (float)freqsets[set2]/numTrans
	       << " - " << canJoin << " -> " 
	       << set << ":" << spt << endl;
#endif
	  freqsets[set] = count;
	}
      } else {
	// continue when set1 and set2 can't join and not equal. 
	if(!(set1 == set2)) break; 
      }
    }
  }
}

// Save frequent itemsets into given file.
bool DataSet::saveFreqItemSets(string fname) {
  ofstream fdata(fname.c_str());
  // Check if open file successful.
  if(!fdata) {
   cerr << "Error opening file to save map data" << endl; 
   return false;
  }
  map<Itemset, int>::iterator it; 
  for(it = freqsets.begin(); it != freqsets.end(); it++) {
    fdata << it->first << ":" << (float)(*it).second/numTrans << endl; 
  }

  fdata.close();
  return true;
}

// Generate Association rules for all the frequent itemsets in the
// hash tree. 
bool DataSet::genAssoRule() {
  map<Itemset, int>::iterator it; 
  for(it = freqsets.begin(); it != freqsets.end(); it++) {
    Itemset set = it->first;
    int count = (*it).second;
    int size = set.getSize(); 
    float support = (float)count/numTrans;
    // For each frequent set, generating rule. 
    if(size == 1) continue; 	// ignore those with one item. 

    map<Itemset, int> mset; 	// set tree within the tree. 
    vector<Itemset> vset; 
    for(int i = 0; i < size; i++) {
      Item item = set[i];
      Itemset tset; 
      tset.pushBack(item);
      if(mset.find(tset) == mset.end()) {
	mset[tset] = 1; 
      }
    }
    // level two. 
    map<Itemset, int>::iterator it1; 
    for(it1 = mset.begin(); it1 != mset.end(); it1++) {
      map<Itemset, int>::iterator it2; 
      for(it2 = it1; it2 != mset.end(); it2++) {
	Itemset set1 = it1->first, set2 = it2->first; 

	// Generate level two set.
	if(!(set1 == set2) 
	   && (set1.getSize() == 1)
	   && (set2.getSize() == 1)) {
	  Itemset jset = set1 + set2;
	  if(!(jset == set)) {

#ifdef DEBUG_ASSORULE
	    cout << "Level Two Set: " << set1 << " + " 
	       << set2 << "<->" << jset << endl;
#endif
	    if(mset.find(jset) == mset.end()) {
	      mset[jset] = 1; 
	    }
	    //vset.push_back(jset);
	  } // if push to vector. 
	} // if set1 != set2
      }	// for inner loop 
    } // for out loop 

    // Other Levels. 
    map<Itemset, int>::iterator it11; 
    for(it11 = mset.begin(); it11 != mset.end(); it11++) {
      map<Itemset, int>::iterator it21;
      for(it21 = it11; it21 != mset.end(); it21++) {
	Itemset set1 = it11->first; 
	Itemset set2 = it21->first;
	bool canJoin = set1.isJoinable(set2);       
	if(canJoin) {
	  Itemset jset = set1.join(set2);
	  // Push to set. 
	  if(mset.find(jset) == mset.end()
	     && !(jset == set)) {
	    mset[jset] = 1; 
	  }
	} else {
	  if(!(set1 == set2)) break; 
	}
      }
    }

    for(it11 = mset.begin(); it11 != mset.end(); it11++) {
      Itemset anteset = it11->first, conset = set; 
      subtract(conset, anteset); 
#ifdef DEBUG_ASSORULE
      cout << anteset << " -> " << conset << endl;
#endif
      if(freqsets.find(set) != freqsets.end()
	 && freqsets.find(anteset) != freqsets.end()
	 && freqsets.find(conset) != freqsets.end()) {
	float supx = (float)freqsets[anteset]/numTrans;
	float supy = (float)freqsets[conset]/numTrans;
	float supxy = (float)freqsets[set]/numTrans;
	float conf = supxy / supx; 
	if(conf >= minConf) {
	  AssoRule rule(anteset, conset, supxy, supx);
	  qrules.push(rule); 
	}
      }
    }
  }
  return true; 
}

// Print association rules. 
void DataSet::printAssoRule() {
  for(int i = 0; i < k; i++) {
    if(!qrules.empty())  {
      AssoRule trule = qrules.top(); 
      cout << trule;
      qrules.pop();
    }
  }
}

////////////////////////////////////////////////////
// Auxilliary functions.
////////////////////////////////////////////////////

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
