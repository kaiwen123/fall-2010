// $Id$
/**
 * @file main.cpp 
 * @brief This is a test program for this project. 
 * @see README. 
 * @warning
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#include <iostream>
#include <cstdlib>
#include <fstream>
#include "Database.h"
using namespace std;

// ---------------------------------------------------------------------
// main() -- Test the function of insert, search, load data from file and
//           save data to file. 
// ---------------------------------------------------------------------
void getCmd(char& choice); 
bool execCmd(char choice);

Database employeedb;		// the Employee database. 

int main() {
  char choice;			// User command choice. 
  do {
    getCmd(choice);
  } while(execCmd(choice));
  return 0;
}

// Prompt user to enter command 
// The command char will be returned through a reference char. 
void getCmd(char& choice) {
  cout << "\nPlease Select Command From Following MENU:" << endl
       << "(I/i)nsert new record" << endl
       << "(L/l)ast name search" << endl
       << "(S/s)ave database to a file" << endl
       << "(R/r)ead database from a file" << endl
       << "(P/p)rint ALL Employee records" << endl
       << "(N/n)umber of Employee records" << endl
       << "(Q/q)uit" << endl
       << endl << "Enter Choice: ";
  cin >> choice; 
}

// Execute user selected command. 
// 'I/i' for inserting new Employee record to the list. 
// 'L/l' for last name search throught the list. 
// 'R/r' read Employee records from data file. 
// 'S/s' save Employee data records into file. 
// 'Q/q' exit this program. 
// If a wrong command was entered, prompt the user to try again. 
bool execCmd(char choice) {
  string var; 
  switch(choice) {
  case 'I':
  case 'i': {
    employeedb.insertNewEmployee();
    return true;
  }
  case 'L':
  case 'l': {
    cout << "Please enter last name: ";
    cin >> var; 
    if(var.compare("")!=0)
      employeedb.findByLastname(var); 
    return true;
  }
  case 'S':
  case 's': {
    employeedb.saveToFile(); 
    return true;
  }
  case 'R':
  case 'r': {
    employeedb.loadFromFile();
    return true;
  }
  case 'P':
  case 'p': {
    employeedb.printAll();
    return true;
  }
  case 'N':
  case 'n': {
    cout << "Total Number of records: " << employeedb.size() << endl; 
    return true; 
  }
  case 'Q':
  case 'q': {
    cout << "Exiting program." << endl;
    return false;
  }
  default: 
    cout << "Wrong command: " << choice << ", please try again!" << endl;
    cout << endl; 
    return true;  
  }
}
