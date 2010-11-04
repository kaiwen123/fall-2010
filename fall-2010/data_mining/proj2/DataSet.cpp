#include "DataSet.h"

DataSet::DataSet(){
  g = k = minSup = gnum = tnum = 0;
  minConf = 0.0;
  hroot = NULL;
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
	td.push_back(Item(tmp[0], col)); 
	line=line.substr(poscomma+1,line.size()-poscomma).c_str();
	// Control number of genes to process. 
	if(col >= getNumGeneToProcess()-1) {
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
  setNumGenes(totalcol + 1);
  setNumTrans(totalrow + 1);

  // Statistics. 
  cout << "Finished loading data. " << endl
       << "Totally loaded " << getNumTrans()
       << " lines and " << getNumGenes()
       << " columns of data." << endl;
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

// Scan level one frequent sets.
void DataSet::scanLevelOne(Itemset set) {
  if(fst.find(set) == fst.end()) {
    fst[set] = 1;
  } else {
    fst[set]++;
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
#ifdef DEBUG_LEVEL_TWO
	cout << " key: " << set1
	     << " count: " << snd[set1]
	     << endl;
#endif
      }
    }
  }
  // Remove itemsets with frequency < minimum Support. 
  map<Itemset, int>::iterator it; 
  for(it = snd.begin(); it != snd.end(); it++) {
    if((*it).second < getMinSupport()) {
#ifdef DEBUG_APRIORI_PRUNE
      cout << "Remove: " << (*it).first 
	   << " - " << (*it).second 
	   << " < " << getMinSupport() << endl;
#endif
      snd.erase((*it).first);
    }
  }
}

// scan itemset and get the number of occurances. 
int DataSet::scanItemset(Itemset& set) {
  int count = 0;
  bool equal = true;
  for(int i = 0; i < getNumTrans(); i++){
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
// nindex is used to store the index of all the nodes of 
// the hash tree.
bool DataSet::doApriori() {
  // Scanning for level two item sets. 
  scanLevelTwo();
  // First we need to build level one two for the tree.
  // iteratively do APRIORI of other levels. 
  hroot = new (nothrow) HashTree();
  map<string, HashNode*> nindex; // node index of tree.
  map<Itemset, int>::iterator it; 
  queue<HashNode*> qapriori;	// A queue for iterative apriori.
  HashNode *prt;		// parent of to-be-inserted node.
  Itemset set;
  for(it = snd.begin(); it != snd.end(); it++) {
    set.clear();
    Itemset iset = const_cast<Itemset&>((*it).first);
    int cnt = (*it).second;
#ifdef DEBUG_HASH_INSERTSET
    cout << "working for Set: " << iset << endl;	
#endif
    for(int i = 0; i < iset.getSize(); i++) {
      Item itm = iset[i]; set.pushBack(itm);
      int hcode = hashfunc(itm.getId());
      int lvl = set.getSize();
      string nkey = set.calcKeyStr(lvl);
      if(nindex.find(nkey) == nindex.end()) { // no node exists.
	HashNode *node = new (nothrow) HashNode();
	if(!node) {cerr << "Can't create node." << endl; return false;}
	nindex.insert(pair<string, HashNode*>(nkey, node));
	node->insertFreqSet(set, cnt);
	node->setNodeLevel(lvl);
	node->setHashKey(nkey);
	if(lvl == 1) {		// level one
	  prt = hroot->getRoot(); 	  
	} else {		// level higher than one.
	  string key = set.calcKeyStr(lvl-1);
	  prt = nindex[key];
	}
#ifdef DEBUG_HASH_INSERTSET
	cout << "CN " << nkey << "->" << prt->getHashKey() << endl;
	cout << "II " << set << ":" << cnt
	     << " NN: " << node->getHashKey() 
	     << "->" << prt->getHashKey() << endl;
#endif
	prt->insertChildrenSet(set); // insert to children sets for joining.
	qapriori.push(node);
	hroot->insertNode(prt, node); 
      } else {			// node already exists.
	// if can't find set, then insert into node.
	if(!nindex[nkey]->findFreqSet(set)){ 
	  nindex[nkey]->insertFreqSet(set, cnt);
	  nindex[nkey]->getParent()->insertChildrenSet(set);
#ifdef DEBUG_HASH_INSERTSET
	  cout << "II " << set << ":" << cnt 
	       << " ND: " << nkey << "->" 
	       << nindex[nkey]->getParent()->getHashKey() << endl;
#endif
	} // if node exist but set not in node.
      } // if node already exists.
    } // for iterate items within itemset. 
  } // for 

  // Now let's iteratively do other levels of join and pruning.
  HashNode *node;
  vector<Itemset> sets; 	// sets after join. 
  while(!qapriori.empty()) {
    // get Hash tree node. 
    node = qapriori.front(); qapriori.pop();
#ifdef DEBUG_APRIORI 
    cout << "working for node : " << node->getHashKey() << endl;
#endif
    // join itemsets in this node. 
    sets.clear();
    if(node->joinSameParentSets(sets)){
      vector<Itemset>::iterator it; 
      for(it = sets.begin(); it != sets.end(); it++) {
	int cnt = scanItemset(*it);
	if(cnt < getMinSupport()) continue; 
	string key = it->calcKeyStr(it->getSize());
	string pkey = it->calcKeyStr(it->getSize()-1); 
	if(nindex.find(pkey) == nindex.end()) {
	  cerr << "Can't find parent of node " << key << endl;
	  return false; 
	}
	prt = nindex[pkey];
	if(nindex.find(key) == nindex.end()) {
	  HashNode *nnode = new (nothrow) HashNode();
	  if(!nnode) {cerr << "Can't create node." << endl; return false;}
	  nindex.insert(pair<string, HashNode*>(key, nnode));
	  nnode->insertFreqSet(*it, cnt);
	  nnode->setNodeLevel(it->getSize());
	  nnode->setHashKey(key);
	  qapriori.push(nnode);
	  prt->insertChildrenSet(*it); ///
	  hroot->insertNode(prt, nnode);
#ifdef DEBUG_HASH_INSERTSET
	  cout << "CN " << key << "->" << node->getHashKey() << endl;
	  cout << "II " << set << ":" << cnt
	       << " NN: " << nindex[key]->getHashKey() << "->" 
	       << prt->getHashKey() << endl;
#endif
	} else {
	  if(!nindex[key]->findFreqSet(*it)){
	    nindex[key]->insertFreqSet(*it, cnt);
	    nindex[key]->getParent()->insertChildrenSet(*it);
#ifdef DEBUG_HASH_INSERTSET
	    cout << "II " << *it << ":" << cnt 
		 << " ND: " << key << "->" 
		 << nindex[key]->getParent()->getHashKey() << endl;
#endif
	  } // if find set in node.
	} // if find node.
      }	// for sets. 
    }
  }
#ifdef DEBUG_APRIORI_TRAVERSAL
  hroot->levelTraverse(hroot->getRoot());
#endif
}

// Save frequent itemsets into given file.
bool DataSet::saveFreqItemSets(string fname) {

}

// Print data set of a certain level. 
void DataSet::printLevelFreqSets(int level){
  switch(level) {
  case 1: {
    map<Itemset, int>::iterator it; 
    for(it = fst.begin(); it != fst.end(); it++) {
      cout << (*it).first << ":" << (*it).second << endl;
    }
  }
  case 2: {
    map<Itemset, int>::iterator it; 
    for(it = snd.begin(); it != snd.end(); it++) {
      cout << (*it).first << ":" << (*it).second << endl;
    }
  }
  default: {
    
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
