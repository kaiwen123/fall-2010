#include "entry.h"

// Initialize the static variable. (pay attention here).
// This static variable is used to keep the id of each 
// entry unique globally. 
int Entry::e_counter = 0; 

// @brief constructor of Entry class, which initializes 
// summary variables and generate a globally unique id. 
Entry::Entry():sk(0),nk(0),wcd(0.0),sk2(0) {
  eid = Entry::e_counter++;
  //  my_tree = NULL;
  child_ptr = NULL;
  level = 0;
  DBG_ENTRY("Entry created");
}

// @brief contruct from tree.
// @param root the root of tree structure. 
// Entry::Entry(CFTree* root):sk(0),nk(0),wcd(0.0),sk2(0) {
//   eid = Entry::e_counter++;
//   level = 0; 
//   my_tree = root; 
//   child_ptr = NULL;
//   DBG_ENTRY("Entry created with root.");
// }

// @brief delete child node of *this* entry. 
// @param none
// @return void. 
void Entry::del_child() {
  if (child_ptr != NULL) {
    //delete child_ptr; 
    child_ptr = NULL;
  }
}

Entry::~Entry() {}

// @brief add a transaction to a cluster. The summry
// information will be updated. 
// @param trans the transaction to insert into Entry. 
// @return *this* entry id. 
int Entry::add_trans(map<string, int>& trans) {
  DBG_ENTRY("Adding trans to entry " + itoa(eid)); 
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
  wcd = (float)sk2 / (float)sk;
  DBG_ENTRY(*this);
  DBG_ENTRY("Added Transaction to entry");
  return eid; 
}

// @brief remove trans from cluster. 
// @param trans The trans to remove from cluster. 
// @return eid of *this* entry. 
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
  if (sk > 0) wcd = (float)sk2 / (float)sk;
  else wcd = 0.0; 
  return eid; 
}

// @brief test the addition and removal of trans. 
// @param type add(0) / remove(1)
// @param trans the transaction to test. 
// @return delta wcd on the addition or removal of trans.
float Entry::test_trans(map<string, int>& trans, t_type type) {
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
  if (ADD == type) {
    return (float)(sk2 + ssk2) / (float)(sk + trans.size()) - wcd; 
  } else if (REMOVE == type) {
    if ((sk - trans.size() == 0) || (nk == 1)) {
      return 0.0; 
    } else {
      return wcd - (float)(sk2 - ssk2) / (float)(sk - trans.size());
    }
  } else {
    cerr << "Unsupport test, only support 0/add and 1/remove test." \
	 << endl; 
    return -1.0;
  }
}

// @brief overloading the operator<< for output.
// @param out the output stream for return. 
// @param en reference to entry for output.
// @return output stream. 
ostream& operator<<(ostream& out, Entry& en) {
  out << "Entry: " << en.getEid() << " summary: " 
      << "sk=" << en.getSk() << " " 
      << "sk2=" << en.getSk2() << " " 
      << "wcd=" << en.getWcd() << endl;
#ifdef OUT_VERBOSE
  map<string, int>::iterator it = en.getItems().begin();
  while(it != en.getItems().end()) {
    out << it->first << ":" << it->second << " "; 
    it++;
  }
  out << endl;
#endif
  return out; 
}

// @brief Test if two entry objects are the same. 
// @param en rhs entry object to compare with *this*.
bool Entry::operator==(Entry& en) {
  if (eid != en.getEid()) return false; 
  if (sk != en.getSk()) return false; 
  if (nk != en.getNk()) return false; 
  if (wcd != en.getWcd()) return false; 
  if (sk2 != en.getSk2()) return false; 
  if (level != en.getLevel()) return false;
  //  if (my_tree != en.get_tree()) return false;
  if (child_ptr != en.get_child()) return false; 
  return true; 
}

// @brief Overloading the copy assignment operator. 
// @param en the rhs operator to copy from. 
// Entry& Entry::operator=(Entry& en) {
//   eid = Entry::e_counter++; 
//   sk = en.getSk(); 
//   nk = en.getNk(); 
//   wcd = en.getWcd(); 
//   sk2 = en.getWcd();
//   my_tree = en.get_tree();
//   child_ptr = en.get_child();
//   level = en.getLevel();
//   DBG_ENTRY("New entry created by copy assignment. ");
// }

Entry& Entry::operator+=(Entry& en) {
  sk += en.getSk(); 
  nk += en.getNk();
  sk2 += en.getSk2() + 2 * sk * en.getSk();
  wcd = sk2 / sk; 
  return *this; 
}

// @brief Another print interface.
// @param none. 
// @return void.
void Entry::pprint() {
  cout << *this; 
}
