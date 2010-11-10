#include "AssoRule.h"

// Constructor.
AssoRule::AssoRule(Itemset an, Itemset co, float sptxy, float sptx)
 :ante(an),cons(co),suptxy(sptxy),suptx(sptx) {}

bool AssoRule::operator<(const AssoRule& rule) const {
  return getSupXY() * getConf() < rule.getSupXY() * rule.getConf();
}

bool operator>(const AssoRule& rule1, const AssoRule& rule2){
return rule1.getSupXY() * rule1.getConf() >
  rule2.getSupXY() * rule2.getConf();
}

// output the Association rule. 
ostream& operator<<(ostream& out, AssoRule& rule) {
  out << rule.getAnte() << "-->" << rule.getCons() 
      << " " << rule.getSupXY() 
      << " " << rule.getSupX()
      << " " << rule.getConf() 
      << " " << rule.getSupXY() * rule.getConf() 
      << endl; 
  return out; 
}
