// -*- C -*-
/**
 * @file Database.h
 * @brief Definition of a Database class. 
 * This class will be responsible for managing employee data. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.1
 */
// $Log$

#ifndef _DatabaseClass_
#define _DatabaseClass_
#include <string>
#include <iostream>
#include "Employee.h"
#include "bst.h"
#include "LinkedSortedList.h" 

using namespace std; 

class Database {
 private:
  LinkedSortedList<Employee> *employee; /* The employee record list. */
  BSTree *index;		       /* employee id index. */

 public:
  Database();
  ~Database(){}
  /* getters. */
  int size() const {return employee->size();}
  BSTree* getDBIndex() const {return index;}
  LinkedSortedList<Employee>* getRecordList() const {return employee;}

  /* setters. */

  // Functions to access Employee object. 
  /**
   * @brief insert a new employee into database.  
   * Employee Class will prompt user for info. 
   * @param none.
   * @return true on success and false on failure. 
   */
  bool insertNewEmployee(); 

  /**
   * @brief Delete Employee record by Eid. 
   * @param eid The Employee Eid to be deleted. 
   * @return true on success and false on failure. 
   */
  void deleteByEid(int eid);

  /**
   * @brief Delete all records within the database.
   * This function is reponsible for deleting all the records within
   * the current database, including deleting the linked list employee
   * records as well as the eid index tree. 
   * @param eid The Employee Eid to be deleted. 
   * @return true on success and false on failure. 
   */
  bool emptyDatabase();

  /**
   * @brief Update the employee eid index tree.
   * @param node BSTree node object. 
   * @return true on success and false on failure. 
   */
  bool updateEidIndex(BTreeNode *node);

  /**
   * @brief Search Employee by last name. 
   * 
   * @param lastname.
   * @return void.
   */
  void findByLastname(string var);

  /**
   * @brief Search Employee by employee Id. 
   * 
   * @param eid The Employee Id. 
   * @return void.
   */
  void findByEid(int eid);
 
  /**
   * @brief Save Database Data into file. 
   * Function will prompt user to enter the name of file to save data
   * to. 
   * @param none.
   * @return true on success and false on failure.
   */
  bool saveToFile();

  /**
   * @brief Load Database Data from file. 
   * Function will prompt user enter filename to load from. 
   * @param none.
   * @return true on success and false on failure.
   */
  bool loadFromFile();

  /**
   * @brief Print all the employee data. 
   * 
   * @param none.
   * @return void.
   */
  void printAll();

  /**
   * @brief Put employee data into output stream. 
   * the output doesn't contain annotations. 
   * @param out Output stream. 
   * @param e Database objected to be output. 
   */
  friend ostream& operator<<(ostream& out, Database& e); 
};
#endif	/* ifdef */
