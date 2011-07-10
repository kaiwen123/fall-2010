#include "config.h"
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
WCD::WCD(string fname){
  transfile = fname; 
  tree = new CFTree();
  DBG_WCD("WCD object initialized");
}

// @brief Ewcd based streaming data clustering algorithm.
// Two phases are involved in this algorithm, the first phase 
// is the clustering tree construction phase, and the second 
// phase is the transaction absorption phase. 
// @param none. 
// @return true on success and false on failure. 
bool WCD::doEwcd() {
  cout << "\n\n============starting Ewcd clustering===========" << endl;
  // for test, please delete after test. 
  // Entry *en = new Entry(); 

  fstream ftrans;
  ftrans.open(transfile.c_str(), ifstream::in);
  if(!ftrans) {
    cerr << "Error opening file to save map data" << endl;
    return false;
  }
  DBG_WCD("Opened transaction input file " + transfile + ".");
  map<string, int> trans; 
  int cnt = 0; 
  while(ftrans) {
    string line;
    getline(ftrans, line); 
    if (ftrans.good()) {
      DBG_WCD(line);
    }
    char_separator<char> sep(" ");
    tokenizer<char_separator<char> > tokens(line, sep);
    BOOST_FOREACH(string t, tokens) {
      DBG_WCD("token:" + t);
      // global items dataset. 
      if (items.find(t) != items.end()) {
	items[t]++; 
      } else {
	items[t] = 1;
      }
      // collect trans dataset. 
      if(trans.find(t) == trans.end()) {
	trans[t] = 1; 
      } else {
	trans[t]++; 
      }
    }

    // testing entry. 
    // en->add_trans(trans); 

    // en->remove_trans(trans); 
  
    // add trans into cftree.
    if (trans.size() > 0) {
      tree->insert_trans(trans);// todo, trans is corrupted. 
      trans.clear();		// ??????
    }
    DBG_WCD("\n~~~~~~~new trans~~~~~~~");
  }

  ftrans.close();
  cout << "\n============Ending Ewcd clustering===========" << endl;
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
    cout << getMembership(cnt++) << " " << line << endl; 
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
  cout << *this << endl;

  // then, output the summary table for each entry. 
  tree->pprint(); 
}

// @brief overloading operator << to output the global freq table. 
// @param out, the output stream reference. 
// @param wcd, reference to a wcd object. 
// @return reference to the output stream object. 
ostream& operator<<(ostream& out, WCD& wcd) {
  map<string, int>::iterator it = wcd.getItemFreqTable().begin();
  out << "Global item frequencies: " << endl;
  while(it != wcd.getItemFreqTable().end()) {
    out << it->first << ":" << it->second << ", "; 
    it++;
  }  
  return out; 
}
