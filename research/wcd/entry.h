#ifndef _Cluster_H_
#define _Entry_H_
#include <string>
#include <iostream>
#include <map>

using namespace std; 

class Entry {
 priavte:
  int cid; 
  int sk; 
  int nk; 
  float wcd; 
  int sk2; 
  bool isLeaf; 
  map<string, int> items; 
  Entry* parent; 		/* parent of current cluster. */
  Entry **children; 		/* childrens of this cluster. */

 public:
  Entry(int id);
  ~Entry();

  inline bool isLeaf() {return isLeaf;}
  inline int getCid() {return cid;}
  inline int getSk() {return sk;}
  inline int getNk() {return nk;}
  inline float getWcd() {return wcd;}
  inline int getSk2() {return sk2;}

  Entry* split(Entry& c);
  bool add_trans(map<string, int>& trans);
  bool remove_trans(map<string, int>& trans);
  float test_trans(map<string, int>& trans);
  bool rand_init();

  /* print */
  ostream& operator<<(osream& os, Entry& cl) {
    out << "Entry summary: " << endl;
    out << cid << ":" << "sk = " << sk << " " 
	<< "nk = " << nk << " " 
	<< "wcd = " << wcd << endl;
  }
};


#endif
