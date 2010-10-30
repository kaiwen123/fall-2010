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
  string removeLastChar();	/* helper function for joining. */

 public:
  /* a ctors. */
  Itemset();
  ~Itemset();

  /* getters. */

  /* setters. */

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
  string join(Itemset& set);

  /**
   * @brief Overloading the << operator to output the content of this
   * itemset. 
   * @param out The output stream for return. 
   * @param set Itemset object to operate on. 
   * @return output stream. 
   */
  friend ostream& operator<<(ostream& out, Itemset& set) {
    /* out << set.getItemset() << endl; */
    /* return out; */ 
  }
};

#endif //ifdef
