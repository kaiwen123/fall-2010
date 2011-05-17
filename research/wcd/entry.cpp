#include "entry.h"

Entry::Entry(int id):eid(id),sk(0),nk(0),wcd(0.0),sk2(0.0) {
  leaf = true; 
  cout << "Entry " << eid << " created..." << endl; 
}
Entry::~Entry() {}

// @brief add a transaction to a cluster. The summry
// information will be updated. 
// @param trans the transaction to insert into Entry. 
// @return the eid of the current entry. 
int Entry::add_trans(map<string, int>& trans) {
  cout << "Adding trans to cluster " << eid << endl; 
  map<string, int>::iterator it = trans.begin(); 
  while(it != trans.end()) {
    string t = it->first; 
    int cnt = it->second; 
    if(items.find(t) == items.end()) {
      items[t] = 1;
    } else {
      items[t] += cnt; 
    }
    sk2 += 2 * items[t] + 1;
    it++;
  }
  sk += trans.size();
  nk++; 
  wcd = sk2 / sk;
  return eid;
}

// @brief remove trans from cluster. 
// @param trans The trans to remove from cluster. 
// @return the eid of current entry. 
int Entry::remove_trans(map<string, int>& trans) {
  cout << "removing trans from cluster " << eid << endl; 
  map<string, int>::iterator it = trans.begin(); 
  while(it != trans.end()) {
    string t = it->first; 
    int cnt = it->second; 
    if(items.find(t) == items.end()) {
      items[t] = 1;
    } else {
      items[t] += cnt; 
    }
    sk2 -= 2 * items[t] + 1;
    it++;
  }
  sk -= trans.size();
  nk--; 
  if (sk > 0) wcd = sk2 / sk;
  else wcd = 0.0; 
  return eid;
}

// @brief test the addition and removal of trans. 
// @param type add(0) / remove(1)
// @param trans the transaction to test. 
// @return delta wcd on the addition or removal of trans.
float Entry::test_trans(map<string, int>& trans, int type) {
  float ssk2 = 0.0; 
  map<string, int>::iterator it = trans.begin(); 
  while(it != trans.end()) {
    string t = it->first; 
    int cnt = it->second; 
    if(items.find(t) == items.end()) {
      ssk2 += 1; 
    } else {
      ssk2 += 2 * items[t] + 1; 
    }
    it++;
  }
  if (0 == type) {
    return (sk2 + ssk2) / (sk + trans.size()) - wcd; 
  } else {
    if ((sk - trans.size() == 0) || (nk == 1)) {
      return 0; 
    } else {
      return wcd - (sk2 - ssk2) / (sk - trans.size());
    }
  }
}

// split of clusters.
Entry* Entry::split() {
  Entry *cl = new Entry(1); 
  return cl;
}

// randomly intialize the cluster. 
bool Entry::rand_init() {

}
