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
WCD::WCD(string fname, int cap, int fo, int level){
  transfile = fname; 
  // initialize two clusters and put them onto tree. 
  tree = new Cftree(cap, fo, level);
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
      itemset.insert(t);	// 
      if(trans.find(t) == trans.end()) {
	trans[t] = 1; 
      } else {
	trans[t]++; 
      }
    }
    // add trans into clusters.
    members[cnt] = insert_trans(trans);
    cnt++; 
  }
  ftrans.close();
  return true; 
}

// @brief phase two of the wcd process. 
// this is the iterative adjustment phase, it test all the 
// transaction over all the clusters and choose the one that
// can maximize the EWCD. 
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
	itemset.insert(t);	// 
	if(trans.find(t) == trans.end()) {
	  trans[t] = 1; 
	} else {
	  trans[t]++; 
	}
      }
      // adjust trans to achieve highest ewcd. 
      members[cnt] = adjust_trans(trans, cnt);
    }
    cnt++;
  }
  ftrans.close();
  return true; 
}

// @brief insert trans into a cluster. 
// @param trans the trans to be operated on. 
// @return the membership of the inserted trans. 
int WCD::insert_trans(map<string, int> &trans) {
  return tree->insert_trans(trans);
}

// @brief adjust the membership of a trans in the cluster. 
// @param trans the transaction to work over. 
// @param tid the id/line number of the transaction in dataset.
// @return the cluster id that this trans belongs to. 
int WCD::adjust_trans(map<string, int> &trans, int tid) {
  int oldeid = members[tid]; 
  // test the trans over entries. 1 means remove trans from entry 
  // first and then test adding entry to all other clusters and 
  // choose the one with highest EWCD gain. 
  int neweid = tree->test_trans(trans, 1, oldeid);
  return tree->adjust_trans(trans, oldeid, neweid); 
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
}

// @brief print information about the WCD object. 
// For more details please refer to the overloaded
// operator <<. 
// @param None. 
// @return None. 
void WCD::pprint() {
  cout << *this << endl; 
}
