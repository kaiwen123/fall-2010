#include "Itemset.h"

// Constructors. 
Itemset::Itemset() {is_items.clear();}
Itemset::Itemset(const Itemset& set):is_items(set.getSet()) {}
Itemset& Itemset::operator=(const Itemset& set) {
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

// Join two itemsets to generate a longer one. 
// It will combine the first string with the last character of the
// other string.
Itemset& Itemset::join(Itemset& set) {
  return pushBack(set.getLastItem());
}

// push an item into the set from back. 
Itemset& Itemset::pushBack(Item& item) {
  is_items.push_back(item);
  return *this;
}

// remove the last char from itemset. 
Item& Itemset::getLastItem() {
  return is_items[getSize()-1];
}

// Output item sets.
ostream& operator<<(ostream& out, const Itemset& set) {
  for(int i = 0; i < set.getSize(); i++) {
    out << set.getSet()[i] << ","; 
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
  bool result = true;
  for(int i = 0; i < set1.getSize(); i++) {
    result = result && (set1.getSet()[i] < set2.getSet()[i]);
    //cout << set1.getSet()[i] << " " << set2.getSet()[i] << endl;
    //if(set1.getSet()[i] >= set2.getSet()[i]) return false;
  }
  cout << set1 << " < " << set2 << " " << result << endl;
  return result;
}

// operator >
bool operator>(const Itemset& set1, const Itemset& set2) {
  if(set1.getSize() != set2.getSize()) return false;
  bool result = true;
  for(int i = 0; i < set1.getSize(); i++) {
    result = result && (set1.getSet()[i] > set2.getSet()[i]);
    cout << set1.getSet()[i] << " " << set2.getSet()[i] << endl;
    //if(set1.getSet()[i] < set2.getSet()[i]) return false;
  }
  return result;
}
