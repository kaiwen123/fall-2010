#include "entry.h"

// Initialize the static variable. (pay attention here).
// This static variable is used to keep the id of each 
// entry unique globally. 
int Entry::e_counter = 0; 

// @brief constructor of Entry class, which initializes 
// summary variables and generate a globally unique id. 
Entry::Entry() {
  sk = 0;
  nk = 0; 
  wcd = 0.0; 
  sk2 = 0; 
  child_ptr = NULL;
  eid = Entry::e_counter++;
  DBG_ENTRY("Entry created");
}

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
  if (trans.size() == 0) {
    cerr << "Error, adding empty trans..." << endl; 
    return -1; 
  }
  DBG_ENTRY("Adding trans to entry " + itoa(eid)); 
  map<string, int>::iterator it = trans.begin(); 
  while(it != trans.end()) {
    string t = it->first; 
    int cnt = it->second; 
    if(items.find(t) == items.end()) {
      items[t] = cnt;
      sk2 += cnt * cnt; 
    } else {
      sk2 += 2 * cnt * items[t] + cnt * cnt; 
      items[t] += cnt; 
    }
    sk += cnt; 
    it++;
  }
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
  if (trans.size() == 0) {
    cerr << "Error, removing empty trans..." << endl; 
    return -1; 
  }
  cout << "removing trans from cluster " << eid << endl; 
  map<string, int>::iterator it = trans.begin(); 
  while(it != trans.end()) {
    string t = it->first; 
    int cnt = it->second; 
    if((items.find(t) == items.end()) || (cnt > items[t])) {
      cerr << "trans is not in entry " << getEid() << endl;
      return -1;
    } else {
      items[t] -= cnt; 
      // remove 0 elements. 
      if(items[t] == 0) {
	items.erase(t);
      }
      sk -= cnt;
    }
    it++;
  }
  nk--; 

  // calculate sk2. 
  sk2 = 0; 
  it = items.begin(); 
  while (it != items.end()) {
    int cnt = it->second; 
    sk2 += cnt * cnt; 
    it++; 
  }

  if (sk > 0) wcd = (float)sk2 / (float)sk;
  else wcd = 0.0; 
  return eid; 
}

// @brief test the addition and removal of trans. 
// @param type add(0) / remove(1)
// @param trans the transaction to test. 
// @return delta wcd on the addition or removal of trans.
float Entry::test_trans(map<string, int>& trans, t_type type) {
  if (trans.size() == 0) {
    cerr << "Error, testing empty trans..." << endl; 
    return -1; 
  }
  int ssk2, ssk; 
  float wwcd; 
  map<string, int>::iterator it; 

  // test adding into entry. 
  if (ADD == type) {
    ssk = sk; 
    ssk2 = sk2; 

    it = trans.begin(); 
    while(it != trans.end()) {
      string t = it->first; 
      int cnt = it->second; 
      if(items.find(t) == items.end()) {
	ssk2 += 1; 
      } else {
	ssk2 += 2 * cnt * items[t] + cnt * cnt; 
      }
      ssk += cnt; 
      it++;
    }
    wwcd = (float)ssk2 / (float)ssk; 
    return wwcd - wcd; 
    // test removing from entry. 
  } else if (REMOVE == type) {
    ssk = sk; 
    ssk2 = sk2; 

    it = trans.begin(); 
    while(it != trans.end()) {
      string t = it->first;
      int cnt = it->second; 
      if (items.find(t) == items.end() || (cnt > items[t])) {
	cerr << "trans is not in entry " << getEid() << endl; 
	return -100; 
      } else {
	ssk -= cnt; 
	ssk2 -= (2 * cnt * ssk + cnt * cnt); 
      }
      it++; 
    }

    wwcd = (float) ssk2 / (float) ssk; 
    return wcd - wwcd;
  } else {
    cerr << "Unsupport test " << endl; 
    return -100.0;
  }
}

// @brief overloading the operator<< for output.
// @param out the output stream for return. 
// @param en reference to entry for output.
// @return output stream. 
ostream& operator<<(ostream& out, Entry& en) {
  out << "Entry: " << en.getEid()
      << "(nk=" << en.getNk() << " " 
      << "sk=" << en.getSk() << " " 
      << "sk2=" << en.getSk2() << " " 
      << "wcd=" << en.getWcd() << ") ";
#ifdef OUT_VERBOSE
  map<string, int>::const_iterator it = en.getItems().begin();
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
  //  if (my_tree != en.get_tree()) return false;
  if (child_ptr != en.get_child()) return false; 
  return true; 
}

// @brief overloading the operator<. 
// @param lhs left hand side operator. 
// @param rhs right hand side operator.
// @return true if lhs < rhs else false.
bool operator< (Entry lhs, Entry rhs) {
  return (lhs.getWcd() < rhs.getWcd());
}

Entry& Entry::operator+=(Entry& en) {
  sk += en.getSk(); 
  nk += en.getNk();

  // copy the items in old entry into new entry. 
  map<string, int>::const_iterator it = en.getItems().begin(); 
  while(it != en.getItems().end()) {
    string t = it->first; 
    int cnt = it->second; 
    if(items.find(t) == items.end()) {
      items[t] = cnt; 
    } else {
      items[t] += cnt;
    }
    it++; 
  }

  // calc sk2. 
  sk2 = 0; 
  it = items.begin(); 
  while(it != items.end()) {
    sk2 += it->second * it->second; 
    it++; 
  }

  wcd = (float)sk2 / (float)sk; 
  return *this; 
}

// @brief Another print interface.
// @param none. 
// @return void.
void Entry::pprint() {
  cout << *this; 
}
