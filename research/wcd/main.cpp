/* 
 * @file This is the implementation of the EWCD clustering algorithm .
 * @Author Simon Guo (gsmsteve@gmail.com)
 * @version Ver 1.0 on Tue May 17 14:17:10 2011
 * @history 
 */
#include "entry.h"
#include "cftree.h"
#include "wcd.h"
#include <iostream>
#include <string>
#include <stdlib.h>
#include <stdio.h>

using namespace std;

void usage(); 

// 
// @brief Implementation of the EWCD transactional data clustering
// algorithm, for more details of the concepts of the SCALE framework,
// please refer to the original paper below.
// SCALE: A Scalable Framework for Ef.ciently Clustering Transactional Data
// Author: Hua Yan, Keke Chen,Ling Liu, Zhang Yi
// 
int main(int argc, const char* argv[]) {
  // paramter handling.
  if(argc != 4) { usage(); return 1; }
  string fname = argv[0]; 
  int cap = atoi(argv[1]);
  int fanout = atoi(argv[2]);
  int level = atoi(argv[3]);

  // wcd object and phase1 and phase2 operations. 
  WCD *wcd = new WCD(fname, cap, fanout, level);

  // phase one.
  wcd->phase1(); 

  // phase two.
  wcd->phase2(2);
  
  // output result. 
  wcd->pprint();

  return 0; 
}

// Usage of the program. 
void usage() {
  cerr << "USAGE:\n./wcd <trans_file> <capacity> <fanout> <maxlevel>\n";
  cerr << "trans_file: file name of transaction.\n"
       << "capacity  : capacity of a cluster." << endl
       << "fanout    : maximum children nodes for nonleaf (internal) node.\n"
       << "maxlevel  : maximum level of tree." << endl;
  return; 
}


