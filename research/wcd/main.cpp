#include "cluster.h"
#include "wcd.h"
#include <iostream>
#include <string>

using namespace std;

void usage(); 

// 
int main(int argc, const char* argv[]) {
  // wcd object 
  if(argc != 2) { usage(); return 1; }
  string fname = argv[0]; 
  int cap = argv[1];
  WCD *wcd = new WCD(fname, cap);
  // phase one assign each transaction onto clusters. 
  wcd->phase1(); 
  
  // phase two will iteratively test the position of each
  // transaction. 
  wcd->phase2();
  wcd->pprint();
  return 0; 
}

// Usage of the program. 
void usage() {
  cerr << "USAGE:\n./wcd <trans_file> <capacity>\n";
  cerr << "trans_file: file name of transaction.\n"
       << "capacity  : capacity of a cluster." << endl;
  return; 
}


