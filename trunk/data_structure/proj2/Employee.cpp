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
  cout << "Please enter last name: "; 
  cin >> last_name; 
  cout << endl << "Please enter first name: "; 
  cin >> first_name; 
  cout << endl << "Please enter department: "; 
  cin >> department; 
  cout << endl << "Please enter phone number: "; 
  cin >> phone_num; 
  cout << endl << "Please enter office building address: "; 
  cin >> office_building; 
  cout << endl << "Please enter office room number: "; 
  cin >> office_num; 
  cout << endl << "Please enter hire date: "; 
  cin >> hire_date; 
  cout << endl << "Please enter email: ";
  cin >> email; 
  cout << endl << "Please enter employee ID: ";
  cin >> eid; 
  cout << endl << "Please enter salary: "; 
  cin >> salary; 
  cout << endl; 
}

// Copy Constructor. 
Employee::Employee(const Employee& e) {
  last_name = getLastName();
  first_name = getFirstName();
  department = getDepartment();
  phone_num = getPhoneNum();
  office_building = getOfficeBld();
  office_num = getOfficeNum();
  hire_date = getHireDate();
  email = getEmail();
  eid = getEid();
  salary = getSalary();
}

// Overloading the >= operator. 
bool Employee::operator>=(Employee& e) {
  return last_name.compare(e.last_name) >= 0 ? true : false; 
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

// overloading << operator. 
ostream& operator<<(ostream& out, Employee& e) {
  out << "Last:   " << e.getLastName() << endl
      << "First:  " << e.getFirstName() << endl
      << "EID:    " << e.getEid() << endl
      << "Salary: " << e.getSalary() << endl
      << "Dept:   " << e.getDepartment() << endl; 
  return out; 
}
