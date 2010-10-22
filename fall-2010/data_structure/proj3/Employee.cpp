// -*- C++ -*-
/**
 * @file Employee.h
 * @brief Definition of a Employee class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#include "Employee.h"

Employee::Employee() {
  cout << "Please enter last name(string): "; 
  cin >> last_name; 
  if((last_name.at(0)<='z') && (last_name.at(0)>='a')) 
    last_name.at(0) = last_name.at(0) - ('a' - 'A');
  cout << "Please enter first name(string): "; 
  cin >> first_name; 
  cout << "Please enter department(string): "; 
  cin >> department; 
  cout << "Please enter phone number(string): "; 
  cin >> phone_num; 
  cout << "Please enter office building address(string): "; 
  cin >> office_building; 
  cout << "Please enter office room number(string): "; 
  cin >> office_num; 
  cout << "Please enter hire date(string): "; 
  cin >> hire_date; 
  cout << "Please enter email(string): ";
  cin >> email; 

  cout << "Please enter employee ID(int): ";
  while (!(cin >> eid) || cin.get() != '\n') {
    cin.clear();
    cin.ignore(1000, '\n');
    cout << "Incorrect entry! Please enter a number for EID: ";
  }

  cout << "Please enter salary(int): ";
  while (!(cin >> salary) || cin.get() != '\n') {
    cin.clear();
    cin.ignore(1000, '\n');
    cout << "Incorrect entry! Please enter a number for salary: ";
  }
  
}

// Constructor with vector. 
Employee::Employee(const vector<string>& ed) {
  last_name = ed.at(0);
  first_name = ed.at(1);
  eid = atoi(ed.at(2).c_str());
  salary = atoi(ed.at(3).c_str());
  department = ed.at(4);
  phone_num = ed.at(5);
  int pos_space = ed.at(6).find(" ", 0);
  int len_str = ed.at(6).length();
  office_num = ed.at(6).substr(0, pos_space);
  office_building = ed.at(6).substr(pos_space+1, len_str - pos_space);
  hire_date = ed.at(7);
  email = ed.at(8);
}

// Copy Constructor. 
Employee::Employee(const Employee& e) {
  last_name = e.getLastName();
  first_name = e.getFirstName();
  department = e.getDepartment();
  phone_num = e.getPhoneNum();
  office_building = e.getOfficeBld();
  office_num = e.getOfficeNum();
  hire_date = e.getHireDate();
  email = e.getEmail();
  eid = e.getEid();
  salary = e.getSalary();
}

// Assignment constructor. 
Employee& Employee::operator=(const Employee& e) {
  if(this == &e)
    return *this; 
  last_name = e.getLastName();
  first_name = e.getFirstName();
  department = e.getDepartment();
  phone_num = e.getPhoneNum();
  office_building = e.getOfficeBld();
  office_num = e.getOfficeNum();
  hire_date = e.getHireDate();
  email = e.getEmail();
  eid = e.getEid();
  salary = e.getSalary(); 
  return *this; 
}

// Following operators are comparing by lastname. 
// Overloading the >= operator. 
bool Employee::operator>=(Employee& e) {
  return getLastName().compare(e.getLastName()) >= 0; 
}
bool Employee::operator<=(Employee& e) {
  return getLastName().compare(e.getLastName()) <= 0; 
}
bool Employee::operator>(Employee& e) {
  return getLastName().compare(e.getLastName()) > 0; 
}
bool Employee::operator<(Employee& e) {
  return getLastName().compare(e.getLastName()) < 0;
}

// Overloading the == operator. 
bool Employee::operator==(Employee& e) {
  return (last_name.compare(e.last_name)==0) && 
    (first_name.compare(e.first_name)==0) &&
    (department.compare(e.department)==0) &&
    (phone_num.compare(e.phone_num)==0) &&
    (office_building.compare(e.office_building)==0) &&
    (office_num.compare(e.office_num)==0) &&
    (hire_date.compare(e.hire_date)==0) &&
    (email.compare(e.email)==0) &&
    (eid == e.eid) &&
    (salary == e.salary); 
}

// overloading >> operator for output.
// with annotations. 
ostream& operator>>(ostream& out, Employee& e) {
  out << "Last:   " << e.getLastName() << endl
      << "First:  " << e.getFirstName() << endl
      << "EID:    " << e.getEid() << endl
      << "Salary: " << e.getSalary() << endl
      << "Dept:   " << e.getDepartment() << endl
      << "Phone:  " << e.getPhoneNum() << endl 
      << "Office: " << e.getOfficeNum() + " " + e.getOfficeBld() << endl
      << "H-Date: " << e.getHireDate() << endl 
      << "Email:  " << e.getEmail() << endl; 
  return out; 
}

// overloading << also for output.
// without annotations. 
ostream& operator<<(ostream& out, Employee& e) {
  out << e.getLastName() << endl
      << e.getFirstName() << endl
      << e.getEid() << endl
      << e.getSalary() << endl
      << e.getDepartment() << endl
      << e.getPhoneNum() << endl 
      << e.getOfficeNum() + " " + e.getOfficeBld() << endl
      << e.getHireDate() << endl 
      << e.getEmail() << endl; 
  return out;
}

void Employee::print() {
  cout << *this;
}

