/* 
 * @file This is the implementation of the EWCD clustering algorithm .
 * @Author Simon Guo (gsmsteve@gmail.com)
 * @revision Ver 1.0 on Tue May 17 14:17:10 2011
 * @revision Ver 2.0 on Wed Jun 29 16:22:55 EDT 2011
 */
#include "wcd.h"
#include <stdlib.h>

using namespace std;

// @brief Implementation of the EWCD transactional data clustering
// algorithm, for more details of the concepts of the SCALE framework,
// please refer to the original paper below.
// SCALE: A Scalable Framework for Ef.ciently Clustering Transactional Data
// Author: Hua Yan, Keke Chen,Ling Liu, Zhang Yi
// The whole EWCD process involves two general phases, the first 
// phase is to assign initial cluster to each transaction in the file
// provided to the program. And the second phase is to iteratively
// ajust the clustering membership for each cluster. 
// @param fname file that contains transactional data. 
// @param fanout the largest number of children for each node. This
// param causes the split of non-leaf nodes. 
// @param level maximum level of cftree, this parameter along with
// fanout can determine the total number of nodes in the cftree. 
// @return 0 or 1. 
int main(int argc, const char* argv[]) {
  // paramter handling.
  if(argc != 4) { usage(); return 1; }
  string fname = argv[1]; 
  int fanout = atoi(argv[2]);
  int level = atoi(argv[3]);

  // wcd object and phase1 and phase2 operations. 
  setDegree(fanout); 
  setMaxLevel(level); 
  setUplimit(); 

  WCD *wcd = new WCD(fname);

  // do ewcd clustering. 
  wcd->doEwcd();
  wcd->pprint(); 
  return 0;
}

