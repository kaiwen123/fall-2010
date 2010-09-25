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
#include "LinkedSortedList.h"
#include "Employee.h"
using namespace std;

// ---------------------------------------------------------------------
// main() -- Load some values into the list and print it.  Then clear
//           the list and reload it.  Finally, print the values in the
//           list using getfirst, which should remove all the values
//           from the list.
// ---------------------------------------------------------------------
void getCmd(char& choice); 
bool execCmd(char choice);
void saveToFile();
void loadFromFile();

LinkedSortedList<Employee> eple; // The employee object. 

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
  cout << "MENU" << endl
       << "(I/i)nsert new record" << endl
       << "(L/l)ast name search" << endl
       << "(S/s)ave database to a file" << endl
       << "(R/r)ead database from a file" << endl
       << "(Q/q)uit" << endl
       << endl << "Enter Choice: ";
  cin >> choice; 
}

// Execute user selected command. 
// If 'Q' was entered, exit this program. 
// If a wrong command was entered, prompt the user to try again. 
bool execCmd(char choice) {
  string var; 
  switch(choice) {
  case 'I':
  case 'i': {
    Employee* em = new Employee();  
    eple.insert(*em);
    return true;
  }
  case 'L':
  case 'l': {
    cout << "Please enter last name: ";
    cin >> var; 
    if(var.compare("")!=0)
      eple.find(var); 
    return true;
  }
  case 'S':
  case 's': {
    saveToFile(); 
    return true;
  }
  case 'R':
  case 'r': {
    loadFromFile();
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

// Save the employee information to file.
void saveToFile() {
  cout << "Saving database......";
  cout << endl; 
}

// Load employee information to file. 
void loadFromFile() {
  cout << "Loading database from file......";
  cout << endl;
}
