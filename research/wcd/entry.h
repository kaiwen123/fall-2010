#ifndef _Entry_H_
#define _Entry_H_
#include <string>
#include <iostream>
#include <map>

using namespace std; 

class Entry {
 private:
  // entry summary information. 
  int eid; 			/* The entry id. */
  int sk; 			/* total item occurences. */
  int nk; 			/* total transactions. */
  float wcd; 			/* weighted clustering density. */
  int sk2; 			/* square sum of item occurences. */
  bool leaf; 			/* if this entry is a leaf node. */
  map<string, int> items; 	/* items summary in this entry. */
  
  // pointers for the cftree structure. 
  Entry* parent; 		/* parent of current entry. */
  map<int, Entry*> children; /* vector of all children for current entry. */

 public:
  Entry(int id);
  ~Entry();

  // getters for class. 
  inline bool isLeaf() {return leaf;}
  inline int getEid() {return eid;}
  inline int getSk() {return sk;}
  inline int getNk() {return nk;}
  inline float getWcd() {return wcd;}
  inline int getSk2() {return sk2;}
  Entry* getParent() {return parent;}
  bool isRoot() {return NULL == parent;}
  map<int, Entry*>& getChildren() {return children;}
  int getChildCount() {return children.size();}

  // setters .
  void setLeaf(){leaf = true;}
  void unsetLeaf() {leaf = false;}
  void setParent(Entry* en) {parent = en;}
  void insertChild(Entry* child) {children[child->getEid()] = child;}
  void eraseChild(int eid) {children.erase(eid);}
  
  // core operations over the entry/cluster.
  Entry* split();
  int add_trans(map<string, int>& trans);
  int remove_trans(map<string, int>& trans);
  float test_trans(map<string, int>& trans, int type);
  bool rand_init();

  // print and output operations.
  friend ostream& operator<<(ostream& out, Entry& en) {
    out << "Entry summary: " << endl;
    out << en.getEid() << ":" << "sk = " << en.getSk() << " " 
	<< "nk = " << en.getNk() << " " 
	<< "wcd = " << en.getWcd() << endl;
    return out; 
  }
};


#endif
