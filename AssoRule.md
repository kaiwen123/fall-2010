# Introduction #

The declaration of the AssoRule algorithm.


# Details #
```

// -*- C -*-
/**
 * @file AssoRule.h
 * @brief Definition of AssoRule class. 
 * 
 * @author Shumin Guo (guo.18@wright.edu)
 * @timestamp Tue Oct 19 00:15:06 EDT 2010
 * @version 1.0.0
 */
// $Log$

#ifndef _AssoRuleClass_
#define _AssoRuleClass_ 
#include "defs.h"
#include "Item.h"
#include "Itemset.h"

using namespace std; 
class AssoRule {
 private:
  Itemset ante, cons; 		/* ante and cons of rule. */
  float suptxy, suptx;  	/* support and confidence. */

 public:
  /* a ctors. */
  AssoRule();
  AssoRule(Itemset an, Itemset co, float sptxy, float sptx);
  ~AssoRule(){}

  /* getters. */
  Itemset& getAnte() {return ante;}
  Itemset& getCons() {return cons;}
  float getSupXY() const {return suptxy;}
  float getSupX() const {return suptx;}
  float getConf() const {return suptxy / suptx;}

  /* setters. */
  void setAnte(Itemset set) {ante = set;}
  void setCons(Itemset set) {cons = set;}
  friend ostream& operator<<(ostream& out, AssoRule& rule);
  bool operator<(const AssoRule& rule) const; 
  bool operator()(const AssoRule& rule1, const AssoRule& rule2) const {
    return rule1.getSupXY() * rule1.getConf() <
      rule2.getSupXY() * rule2.getConf();
  }
};
bool operator>(const AssoRule& rule1, const AssoRule& rule2);

#endif //ifdef


```