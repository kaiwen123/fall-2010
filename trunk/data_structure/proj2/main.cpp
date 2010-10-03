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
#include "LinkedSortedList.h"
#include "Employee.h"
using namespace std;

// ---------------------------------------------------------------------
// main() -- Test the function of insert, search, load data from file and
//           save data to file. 
// ---------------------------------------------------------------------
void getCmd(char& choice); 
bool execCmd(char choice);
void saveToFile();
void loadFromFile();
void printAll();

LinkedSortedList<Employee> eple; // The employee list object. 

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
    Employee em;
    eple.insert(em);
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
  case 'P':
  case 'p': {
    printAll();
    return true;
  }
  case 'N':
  case 'n': {
    cout << "Total Number of records: " << eple.size() << endl; 
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
  string fname; 
  cout << "Please Enter the File Name to Save TO: ";
  cin >> fname; 
  cout << "Saving records to file " << fname << "..." << endl; 
  if(eple.saveToFile(fname))
    cout << "Totally saved " << eple.size() 
	 << " records to " << fname << endl;
}

// Load employee information to file. 
void loadFromFile() {
  string fname; 
  cout << "Please Enter file name to read from: ";
  cin >> fname;
  string line; int num = 0;
  fstream fd;
  vector<string> edata;		// Employee data. 
  fd.open(fname.c_str(), fstream::in);
  if(!fd) {cerr << "Error opening data file..." << endl; return;}
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
	  return; // end of file 
	}
	edata.push_back(line);
      }
      // Now creating Employee object and insert into list.
      Employee em(edata);
      eple.insert(em); num++; 
    } //if 
  } //while  
}

// print out all the records to stdout.
void printAll() {
  eple.print();
}
