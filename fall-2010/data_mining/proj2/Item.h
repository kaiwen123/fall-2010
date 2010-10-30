// -*- C -*-
/**
 * @file Item.h
 * @brief Definition of a Item class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#ifndef _ItemClass_
#define _ItemClass_ 
#include "defs.h"

using namespace std; 

class Item {
 private: 
  //char i_class;			/* Anotated gene class value */
  char i_group;			/* Group information 'a,b,c,d'*/
  int i_id;			/* unique item id. */

 public:
 Item(char grp):i_group(grp){cout << "item: "<<grp << endl;}
  ~Item(){}
  
  /* getter */
  //char getClass()const{return i_class;}
  char getGroup()const{return i_group;}
  int getId() const {return i_id;}

  /* setter */
  //void setClass(char c){i_class = c;}
  void setGroup(char g){i_group = g;}
  void setId(int id){i_id = id;}

  /**
   * @brief Overloading comparison operators. 
   * @param item rhs Item object. 
   * @return if *this* object is (==, >, <, >=, <=) rhs object, then
   * return true, otherwise return false.
   */
  bool operator==(Item& item){return getId() == item.getId();}
  bool operator>(Item& item){return getId() > item.getId();}
  bool operator<(Item& item){return getId() < item.getId();}
  bool operator>=(Item& item){return getId() >= item.getId();}
  bool operator<=(Item& item){return getId() <= item.getId();}

  /**
   * @brief Overloadiing the << operator. 
   * @param out output stream. 
   * @param item Item Object. 
   * @return output stream. 
   */
  friend ostream& operator<<(ostream& out, Item const& item) {
    out << item.getGroup();
    return out;
  } 
};

#endif //ifdef
