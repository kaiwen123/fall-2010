// -*- C++ -*-
/**
 * @file Database.cpp
 * @brief Implementation of the Database class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#include "Database.h"

Database::Database() {
  index = new (nothrow) BSTree(); // create employee id index. 
  if(!index) {
    cerr << "Error while creating employee index." << endl;
  }
  employee = new (nothrow) LinkedSortedList<Employee>;
  if(!employee) {
    cerr << "Error while creating employee record list." << endl;
  }
}

// overloading << also for output.
// without annotations. 
ostream& operator<<(ostream& out, Database& e) {
  return out;
}

// Save the employee information to file.
bool Database::saveToFile() {
  string fname; 
  cout << "Please Enter the File Name to Save TO: ";
  cin >> fname; 
  cout << "Saving records to file " << fname << "..." << endl; 
  if(employee->saveToFile(fname))
    cout << "Totally saved " << employee->size() 
	 << " records to " << fname << endl;
  return true;
}

// Load employee information from file. 
bool Database::loadFromFile() {
  // Empty the employee database if it is not empty yet.
  if(employee->size() > 0) { 
    emptyDatabase();
  }

  // Now, start loading from file. 
  string fname; 
  cout << "Please Enter file name to read from: ";
  cin >> fname;
  string line; int num = 0;
  fstream fd;
  vector<string> edata;		// Employee data. 
  fd.open(fname.c_str(), fstream::in);
  if(!fd) {
    cerr << "Error opening data file..." << endl; 
    return false;
  }
  cout << "Loading records from file " << fname << "..." << endl; 
  while(fd) {
    getline(fd, line, fd.widen('\n'));
    // Start of file or start of employee data. 
    if((line.compare("<Records>")==0) || (line.compare("--")==0)) {
      edata.clear();
      for(int i = 0; i < 9; i++) {
	getline(fd, line, fd.widen('\n'));
	if(line.compare("<END>")==0) {
	  cout << "Loading finished, totally loaded " 
	       << num << " records. "<< endl; 
	  //index->preOrderTraverse(index->getRoot());
	  //index->inOrderTraverse(index->getRoot());
	  //index->postOrderTraverse(index->getRoot());
	  return true; // end of file 
	}
	edata.push_back(line);
      }
      // Now creating Employee object and insert into list.
      Employee em(edata);
      LinkedNode<Employee> *enode = 
	new (nothrow) LinkedNode<Employee>(em); 
      if(!enode) {
	cerr << "Error creating Employee Node. " << endl;
	return false; 
      }
      BTreeNode *bnode = new (nothrow) BTreeNode(enode);
      if(!bnode) return false;
      if(updateEidIndex(bnode)) {
	employee->insert(enode); 
	num++; 
      } else {
	delete bnode, enode; 
      }
    } //if 
  } //while 
  return true; 
}

// Delete all the records within the database. 
bool Database::emptyDatabase() {
  cout << "Clearing Employee Records in list ...";
  if(employee->emptyList()) {
    cout << " done!" << endl; 
  } else {return false;}

  cout << "Clearing index ...";
#ifdef DEBUG_DELETE_TREE
  cout << "Before index Deletion..." << endl;
  index->inOrderTraverse(index->getRoot());
#endif
  index->destroyTree(index->getRoot());
  cout << " done!" << endl;
#ifdef DEBUG_DELETE_TREE
    cout << "size of tree now is: " << index->getSize() << endl; 
    cout << "After index Deletion..." << endl;
    index->inOrderTraverse(index->getRoot());
#endif
  return true;
}

// print out all the records to stdout.
void Database::printAll() {
  employee->print();
}

// Insert new Employee into database. 
bool Database::insertNewEmployee() {
  Employee e;
  LinkedNode<Employee> *enode = new (nothrow) LinkedNode<Employee>(e); 
  if(!enode) {
    cerr << "Error creating Employee Node. " << endl;
    return false; 
  }
  BTreeNode *bnode = new (nothrow) BTreeNode(enode);
  if(!bnode) return false;
  if(updateEidIndex(bnode)) {
    employee->insert(enode);
  } else {
    delete enode, bnode;
  }
}

// Update Eid index tree. 
// This function will try to insert the newly created node into the
// index tree, which will also do duplicates tests.
bool Database::updateEidIndex(BTreeNode *node) {
  return index->insertNode(index->getRoot(), node);
}

// Search employee by lastname. 
void Database::findByLastname(string var) {
  employee->find(var);
}

// Search employee record by eid, this is used to test if there is
// duplicates if this item is inserted into the list. 
void Database::findByEid(int eid) {
  cout << "Searching...... " << endl;
  index->findByEid(eid);
}

// Delete employee record by Eid. 
void Database::deleteByEid(int eid) {
  index->deleteByEid(employee, eid);
}
