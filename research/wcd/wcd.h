#ifndef _WCD_H_
#define _WCD_H_
#include "cftree.h"

class CFTree;

using namespace std; 

class WCD {
 private:
  // cftree params. 
  // transaction params.
  string transfile;		/* filename of trans. */
  // vector<int> members;		/* membership for each trans. */
  map<string, int> items;	/* all items appear in trans. */
  CFTree *tree;			/* tree to host clusters. */

 public:
  WCD(string fname);
  bool doEwcd(); 
  //bool phase2(int iter);

  // getters.
  map<string, int>& getItemFreqTable() {return items;}
  //void addMembership(int eid);

  /* output */
  void tprint();		/* print labeled transaction */
  void pprint(); 
  friend ostream& operator<<(ostream& out, WCD& wcd);
}; 

#endif
