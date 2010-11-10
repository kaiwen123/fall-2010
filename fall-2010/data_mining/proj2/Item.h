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
  int col;			/* which column does this object reside. */
  char i_group;			/* Group information 'a,b,c,d'*/
  int i_id;			/* unique item id. */

 public:
 Item(char grp, int cpos):i_group(grp),col(cpos){
#ifdef DEBUG_DATA_LOADI   
    cout << "created item: "<< grp << " col: " << col << endl;
#endif
  }
  Item(const Item& item) {
    col = item.getColumnPos();
    i_group = item.getGroup();
    i_id = item.getId();
  }
  ~Item(){}
  
  /* getter */
  char getGroup()const{return i_group;}
  int getColumnPos() const {return col;}
  int getId() const {return i_id;}

  /* setter */
  void setGroup(char g){i_group = g;}
  void setId(int id){i_id = id;}
  void setColumnPos(int cpos) {col = cpos;}
  Item& operator=(const Item& item){
    if(this == &item) return *this;
    col = item.getColumnPos();
    i_group = item.getGroup();
    i_id = item.getId();
    return *this;
  }
  /**
   * @brief Overloading comparison operators. 
   * @param item rhs Item object. 
   * @return if *this* object is (==, >, <, >=, <=) rhs object, then
   * return true, otherwise return false.
   */
  bool operator==(Item& item){return getId() == item.getId();}
  bool operator!=(Item& item){return !(*this == item);}
  bool operator>(Item& item){return getId() > item.getId();}
  bool operator<(Item& item){return getId() < item.getId();}
  bool operator>=(Item& item){return !(*this < item);}
  bool operator<=(Item& item){return !(*this > item);}
  friend ostream& operator<<(ostream& out, Item const& item) {
    out << /* item.i_group << " " << */ item.getId();
    return out;
  } 
};

#endif //ifdef
