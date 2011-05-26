#ifndef _WCD_H_
#define _WCD_H_
#include "cftree.h"

using namespace std; 

class WCD {
 private:
  // cftree params. 
  // transaction params.
  string transfile;		/* filename of trans. */
  vector<int> members;		/* membership for each trans. */
  map<string, int> items;	/* all items appear in trans. */
  CFTree *tree;			/* tree to host clusters. */

 public:
  WCD(string fname, int fo, int maxentry, int level);
  bool phase1(); 
  bool phase2(int iter);

  // getters.
  map<string, int>& getItemFreqTable() {return items;}

  /* output */
  void tprint();		/* print labeled transaction */
  void pprint(); 
  friend ostream& operator<<(ostream& out, WCD& wcd);
}; 

#endif
