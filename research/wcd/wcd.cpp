#include "wcd.h"
#include <iostream>
#include <fstream>
#include <boost/foreach.hpp>
#include <boost/tokenizer.hpp>

using namespace boost; 

// @brief constructor of the WCD class. 
// @param fname the filename that contains the trans data. 
// @param cap the maximum number of trans a leaf entry can host.
// @param fo the fanout of the nonleaf node.
// @param level the maximum level of clustering tree. 
WCD::WCD(string fname, int fo, int maxentries, int level){
  transfile = fname; 
  tree = new CFTree(fo, maxentries, level);
}

// @brief phase one of the wcd process, all trans will be 
// assigned to a particular cluster, and cluster label is 
// store onto the members vector. 
// @param none. 
// @return true on success and false on failure. 
bool WCD::phase1() {
  cout << "starting phase one...." << endl; 
  ifstream ftrans;
  ftrans.open(transfile.c_str(), ifstream::in);
  if(!ftrans) {
    cerr << "Error opening file to save map data" << endl;
    return false;
  }
  map<string, int> trans; 
  int cnt = 0; 
  while(ftrans) {
    string line;
    getline(ftrans, line); 
    char_separator<char> sep(" ");
    tokenizer<char_separator<char> > tokens(line, sep);
    BOOST_FOREACH(string t, tokens) {
      cout << t << "." << endl;
      if (items.find(t) != items.end()) {
	items[t]++; 
      } else {
	items[t] = 1;
      }
      // items.insert(t);	// 
      if(trans.find(t) == trans.end()) {
	trans[t] = 1; 
      } else {
	trans[t]++; 
      }
    }
    // add trans into entries.
    members[cnt++] = tree->insert_trans(trans);
  }
  ftrans.close();
  return true; 
}

// @brief phase two of the wcd process. 
// this is the iterative membership adjustment phase, for each trans,
// it test the ewcd increment over all the clusters and choose the one that
// can maximize the EWCD. 
// currently, I only adjust membership among the leaf node entries. 
// @param iter number of adjustive iterations. 
// @return true on success and false on failure. 
bool WCD::phase2(int iter) {
  cout << "Staring phase two..." << endl; 
  ifstream ftrans; 
  ftrans.open(transfile.c_str(), ifstream::in);
  map<string, int> trans;
  for(int i = 0; i < iter; i++) {
    int cnt = 0; 		// trans line number;
    while(ftrans) {
      string line;
      getline(ftrans, line); 
      char_separator<char> sep(" ");
      tokenizer<char_separator<char> > tokens(line, sep);
      BOOST_FOREACH(string t, tokens) {
	cout << t << "." << endl;
	if (items.find(t) != items.end()) {
	  items[t]++; 
	} else {
	  items[t] = 1;
	}	
	if(trans.find(t) == trans.end()) {
	  trans[t] = 1; 
	} else {
	  trans[t]++; 
	}
      }
      // adjust trans to achieve highest ewcd. 
      members[cnt] = tree->adjust_trans(trans, members[cnt]);
    }
  }
  ftrans.close();
  return true; 
}

// @brief print labeled transactions. 
// @param None
// @return none. 
void WCD::tprint() {
  ifstream ftrans;
  ftrans.open(transfile.c_str(), ifstream::in);
  int cnt = 0; 
  while(ftrans) {
    string line;
    getline(ftrans, line); 
    cout << members[cnt++] << " " << line << endl; 
  }
  ftrans.close();
}

// @brief print information about the WCD object. 
// For more details please refer to the overloaded
// operator <<. 
// @param None. 
// @return None. 
void WCD::pprint() {
  // first output the global summary table. 
  cout << *this;

  // second, output the summary table for each entry. 
  tree->pprint(); 
}

// @brief overloading operator << to output the global freq table. 
// @param out, the output stream reference. 
// @param wcd, reference to a wcd object. 
// @return reference to the output stream object. 
ostream& operator<<(ostream& out, WCD& wcd) {
  map<string, int>::iterator it = wcd.getItemFreqTable().begin();
  cout << "global item frequencies: " << endl; 
  while(it != wcd.getItemFreqTable().end()) {
    out << it->first << " : " << it->second << endl; 
  }  
  return out; 
}