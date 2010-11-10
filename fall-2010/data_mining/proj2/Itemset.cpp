#include "Itemset.h"

// Constructors. 
Itemset::Itemset():count(0) {is_items.clear();}
Itemset::Itemset(const Itemset& set):is_items(set.getSet()),count(getCount()) {}
Itemset& Itemset::operator=(const Itemset& set) {
  if(this == &set) return *this;
  is_items = set.getSet();
  count = set.getCount();	// count of itemset. 
  return *this;
}

// Destructor. 
Itemset::~Itemset(){is_items.clear();}

// test if *this* itemset is joinable with another one.
bool Itemset::isJoinable(Itemset& set) {
  if(getSize() == 1 || set.getSize() == 1) return false; 
  if(getSize() != set.getSize()) return false;
  if(*this == set) return false;
  for(int i = 0; i < getSize() - 1; i++) {
    if(is_items[i] != set[i]) return false;
  }
  return true;
}

bool isJoinable(Itemset& set1, Itemset& set2) {
  if(set1.getSize() == 1 || set2.getSize() == 1) return false; 
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
Itemset Itemset::join(Itemset& set) {
  Itemset rset;
  if(*this < set) {
    rset = *this; Item item = set.getLastItem();
    return rset.pushBack(item);
  } else {
    rset = set; Item item = this->getLastItem();
    return rset.pushBack(item);
  }
}

// push an item into the set from back. 
Itemset& Itemset::pushBack(Item& item) {
  Item itm = item; 
  is_items.push_back(itm);
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
      out << ","; 
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
  //if(set1.getSize() != set2.getSize()) return false;
  int size1 = set1.getSize(), size2 = set2.getSize();
  if(size1 < size2) {
#ifdef DEBUG_ITEMSET_COMPARE
    cout << set1 << " < " << set2 << endl;
#endif
    return true; 
  }
  if(size1 > size2){
#ifdef DEBUG_ITEMSET_COMPARE
    cout << set1 << " > " << set2 << endl;
#endif
    return false;
  }
  
  // When size1 == size2, compare element by element. 
  bool result = false;
  for(int i = 0; i < size1; i++) {
    if(set1.getSet()[i] == set2.getSet()[i]) {
      continue;
    } 
    if(set1.getSet()[i] < set2.getSet()[i]) {
#ifdef DEBUG_ITEMSET_COMPARE
      cout << set1 << " < " << set2 << endl;
#endif
      return true;
    }
    if(set1.getSet()[i] > set2.getSet()[i]) {
#ifdef DEBUG_ITEMSET_COMPARE
      cout << set1 << " > " << set2 << endl;
#endif
      return false;
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

Itemset Itemset::operator+(Itemset& set) {
  Itemset rset = *this; 
  for(int i = 0; i < set.getSize(); i++) {
    Item item = set[i];
    rset.pushBack(item);
  }
  return rset;
}

// Overloading operator - .
bool subtract(Itemset& set1, Itemset& set2) {
  if(set1.getSize() <= set2.getSize()) return false;
  Itemset rset;
  for(int i = 0; i < set1.getSize(); i++) {
    Item item = set1.getSet()[i]; 
    // Test if item is in set. 
    if(!set2.find(item)) {
      rset.pushBack(item);
    }
  }
  set1 = rset; 
  return true;
}
