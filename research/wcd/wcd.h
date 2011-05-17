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
  bool phase2(int iter);

  /* get operations. */
  inline string getTransFile() {return transfile;}

  // transaction operations.
  int insert_trans(map<string, int>& trans); 
  int adjust_trans(map<string, int>& trans, int tid); 
  //int test_trans(map<string, int>& trans, int type, int member);

  /* output */
  void tprint();		/* print labeled transaction */
  void pprint(); 
  friend ostream& operator<<(ostream& out, WCD& wcd) {
    out << "wcd: " << wcd.transfile << endl; 
    return out; 
  }
}; 

#endif
