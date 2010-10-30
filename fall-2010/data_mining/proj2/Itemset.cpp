#include "Itemset.h"

// Constructors. 
Itemset::Itemset():i_set("") {}
Itemset::Itemset(string set):i_set(set) {}

// Destructor. 
Itemset::~Itemset(){}

// test if *this* itemset is joinable with another one.
bool isJoinable(Itemset& set) {
  int len = set.size();
  int ilen = getLevel(); 	// level == length here.

  // test if the length are equal and larger than two.
  if(len != ilen || len < 2 || ilen < 2) return false;

  // Then test if the first n-1 characters are equal. 
  string substr1 = getItemset().substr(0, ilen - 1); 
  string substr2 = set.substr(0, len - 1);

#ifdef DEBUG_ITEMSET
  cout << "Joinable Test: " << getItemset() << " " 
       << set << endl;  
#endif

  if(substr1.compare(substr2) == 0) return true; 
  return false; 
}

// Join two itemsets to generate a longer one. 
// It will combine the first string with the last character of the
// other string.
string join(Itemset& set) {
  return getItemset() + set.substr(set.at(set.size()-1));
}

// remove the last char from itemset. 
string removeLastChar() {
  if(i_set.size() >= 1) {
    return i_set.substr(0, i_set.size() - 1);
  }
  return "";
}
