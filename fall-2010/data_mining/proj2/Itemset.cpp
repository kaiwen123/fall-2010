#include "Itemset.h"

// Constructors. 
Itemset::Itemset() {is_items.clear();}
Itemset::Itemset(const Itemset& set):is_items(set.getSet()) {}
Itemset& Itemset::operator=(const Itemset& set) {
  if(this == &set) return *this;
  is_items = set.getSet();
  return *this;
}

// Destructor. 
Itemset::~Itemset(){}

// test if *this* itemset is joinable with another one.
bool Itemset::isJoinable(Itemset& set) {
  if(getSize() != set.getSize()) return false;
  for(int i = 0; i < getSize() - 1; i++) {
    if(is_items[i] != set[i]) return false;
  }
  return true;
}

bool isJoinable(Itemset& set1, Itemset& set2) {
  if(set1.getSize() != set2.getSize()) return false;
  if(set1 == set2) return false;
  for(int i = 0; i < set1.getSize() - 1; i++) {
    if(set1[i] != set2[i]) return false;
  }
  return true;
}

// Join two itemsets to generate a longer one. 
// It will combine the first string with the last character of the
// other string.
//Itemset& Itemset::join(Itemset& set) {
Itemset Itemset::join(Itemset& set) {
  Itemset rset = *this;Item item = set.getLastItem();
  return rset.pushBack(item);
  //return pushBack(set.getLastItem());
}

// push an item into the set from back. 
Itemset& Itemset::pushBack(Item& item) {
  is_items.push_back(item);
  return *this;
}

// get the last char from itemset. 
Item Itemset::getLastItem() {
  assert(getSize() >= 1);
  return is_items[getSize()-1];
}
// get second last item from sets. 
Item Itemset::getSndLastItem() {
  assert(getSize() >= 2);
  return is_items[getSize()-2];;
}

// calculate the string for the use by map. 
// format: 0/1/23/34/34
string Itemset::calcKeyStr(int level) {
  if(getSize() == 0 || level == 0) return "";
  string key = itoa(level) + ":" + itoa(hashfunc(is_items[0].getId()));
  for(int i = 1; i < level; i++) {
    key += "/" + itoa(hashfunc(is_items[i].getId()));
  }
  return key;
}
// Output item sets.
ostream& operator<<(ostream& out, const Itemset& set) {
  for(int i = 0; i < set.getSize(); i++) {
    out << set.getSet()[i];
    if(i+1 < set.getSize()) {
      cout << ","; 
    }
  }
  return out; 
}

// Equality test. 
bool Itemset::operator==(Itemset& set) {
  if(getSize() != set.getSize()) return false;
  for(int i = 0; i < getSize(); i++) {
    if(is_items[i] != set[i]) return false;
  }
  return true;
}

// operator <
bool operator<(const Itemset& set1, const Itemset& set2) {
  if(set1.getSize() != set2.getSize()) return false;
  bool result = false;
  for(int i = 0; i < set1.getSize(); i++) {
    if(set1.getSet()[i] == set2.getSet()[i]) {
      continue;
    } else {
      if(set1.getSet()[i] < set2.getSet()[i]) {
	result = true; break;
      } else {
	result = false; break;
      }
    }
  }
  return result;
}

// operator >
bool operator>(const Itemset& set1, const Itemset& set2) {
  if(set1.getSize() != set2.getSize()) return false;
  bool result = true;
  for(int i = 0; i < set1.getSize(); i++) {
    result = result && (set1.getSet()[i] > set2.getSet()[i]);
    cout << set1.getSet()[i] << " " << set2.getSet()[i] << endl;
  }
  return result;
}
