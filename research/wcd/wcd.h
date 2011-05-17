#ifndef _WCD_H_
#define _WCD_H_
#include "cftree.h"
#include <set>

using namespace std; 

class WCD {
 private:
  // cftree params. 
  // transaction params.
  string transfile;		/* filename of trans. */
  vector<int> members;		/* membership for each trans. */
  set<string> itemset; 		/* all items appear in trans. */
  Cftree *tree;			/* tree to host clusters. */

 public:
  WCD(string fname, int cap, int fo, int level);
  bool phase1(); 
  bool phase2();

  /* get operations. */
  inline string getTransFile() {return transfile;}

  /* add transaction into cluster. 
   * test all the clusters and insert or remove from cluster to
   * achieve the largest EWCD. 
   */
  bool insert_trans(map<string, int>& trans); 
  bool remove_trans(map<string, int>& trans); 

  /* output */
  void tprint();		/* print labeled transaction */
  void pprint(); 
  inline ostream& operator<<(ostream& out, WCD& wcd) {
    out << "wcd: " << wcd.getCapacity() << endl; 
    return out; 
  }
}; 

#endif
