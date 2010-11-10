// -*- C -*-
/**
 * @file Itemset.h
 * @brief Definition of Itemset class. 
 * Use DEBUG_ITEMSET for debugging purposes.
 * @author Shumin Guo (guo.18@wright.edu)
 * @timestamp Tue Oct 19 00:15:06 EDT 2010
 * @version 1.0.0
 */
// $Log$

#ifndef _ItemsetClass_
#define _ItemsetClass_ 
#include "defs.h"
#include "Item.h"

using namespace std; 
class Itemset {
 private:
  vector<Item> is_items;	/* a set of items. */
  int count; 			/* number of occurances in datasets. */

 public:
  /* a ctors. */
  Itemset();
  Itemset(const Itemset& set);
  Itemset& operator=(const Itemset& set);
  ~Itemset();

  /* getters. */
  int getSize() const {return is_items.size();} 
  int getCount() const {return count;}
  vector<Item> getSet() const {return is_items;}
  Item getLastItem();		/* helper function for joining. */
  Item getSndLastItem();	/* get second last. */
  string calcKeyStr(int level);		/* used to calculate the key string. */

  /* setters. */
  Itemset& pushBack(Item& item); /* add item to set. */
  bool clear(){is_items.clear();} /* remove all sets. */
  void setCount(int cnt) {count = cnt;}

  /**
   * @brief Test if this item set is joinable with another one? 
   * @param set The OTHER set that is to test with *this* one. 
   * @return true on yes and false on no.
   */
  bool isJoinable(Itemset& set); 

  /**
   * @brief Join this itemset with another itemset to produce a one
   * level higher itemset.
   * @param set The OTHER set that is to join with *this* one. 
   * @return true on yes and false on no.
   */
  Itemset join(Itemset& set);

  /**
   * @brief Overloading the << operator to output the content of this
   * itemset. 
   * @param out The output stream for return. 
   * @param set Itemset object to operate on. 
   * @return output stream. 
   */
  friend ostream& operator<<(ostream& out, const Itemset& set);
  bool operator==(Itemset& set);
  Itemset operator+(Itemset& set);
  Item& operator[](int idx){return is_items[idx];}
  bool find(Item& item) {
    for(int i = 0; i < getSize(); i++) {
      if(getSet()[i] == item) return true; 
    }
    return false;
  }
};
bool operator<(const Itemset& set1, const Itemset& set2);
bool operator>(const Itemset& set1, const Itemset& set2);
bool isJoinable(Itemset& set1, Itemset& set2);
bool subtract(Itemset& set1, Itemset& set2);
#endif //ifdef
