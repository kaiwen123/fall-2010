// -*- C++ -*-
/**
 * @file Employee.h
 * @brief Definition of a Employee class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#ifndef _EmployeeClass_
#define _EmployeeClass_
#include <string>
#include <vector>
#include <stdlib.h>
#include <iostream>

using namespace std; 

class Employee {
 private:
  string last_name; 
  string first_name; 
  int eid; 
  int salary; 
  string department; 
  string phone_num;
  string office_building; 
  string office_num; 
  string hire_date; 
  string email; 

 public:
  Employee();
  Employee(const vector<string>& ed); 
  Employee(const Employee& e);
  ~Employee(){}
  /* getters. */
  string getLastName() const {return last_name;}
  string getFirstName() const {return first_name;}
  string getDepartment() const {return department;}
  string getPhoneNum() const {return phone_num;}
  string getOfficeBld() const {return office_building;}
  string getOfficeNum() const {return office_num;}
  string getHireDate() const {return hire_date;}
  string getEmail() const {return email;}
  int getEid() const {return eid;}
  int getSalary() const {return salary;}

  /* setters. */
  void setLastName(string lname) {last_name = lname;}
  void setFirstName(string fname) {first_name = fname;}
  void setDepartment(string dpt) {department = dpt;}
  void setPhoneNum(string phone) {phone_num = phone;}
  void setOfficeBld(string office_bld) {office_building = office_bld;}
  void setOfficeNum(string office_n) {office_num = office_n;}
  void setHireDate(string hdate) {hire_date = hdate;}
  void setEmail(string mail) {email = mail;}
  int setEid(int e_id) {eid = e_id;}
  int setSalary(int slry) {salary = slry;}

  /**
   * @brief Compare the last name of two Employee object. 
   * @param A The first employee object. 
   * @param B The second employee object.
   * @return 1 if A.last_name > B.last_name;
   * @return 0 if A.last_name == B.last_name;
   * @return -1 if A.last_name < B.last_name.
   */
  int compareLname(Employee& A, Employee& B); 

  /**
   * @brief Compare the designated field of two Employee object. 
   * @param f The field to be compared, includes (I,L,S,R).
   * @param A The first employee object. 
   * @param B The second employee object.
   * @return 1 if A.f > B.f;
   * @return 0 if A.f == B.f;
   * @return -1 if A.f < B.f.
   */
  int compareLname(char f, Employee& A, Employee& B);

  /**
   * @brief Put employee data into output stream. 
   * the output doesn't contain annotations. 
   * @param out Output stream. 
   * @param e Employee objected to be output. 
   */
  friend ostream& operator<<(ostream& out, Employee& e); 

  /**
   * @brief Put employee data into output stream.
   * Note, normally >> is used as input operator rather than output
   * , so here I only use for convenience. And in order to remove 
   * obsecureness, please follow common sense. Let me know if you 
   * have other not-so misleading operators to use. Thanks. 
   * @param out Output stream. 
   * @param e Employee objected to be output. 
   */
  friend ostream& operator>>(ostream& out, Employee& e); 

  /**
   * @brief Overloading the >= operator according to the last name.
   * rhs: right hand side AND lhs: left hand side.
   * @param rhs Right hand side operant  
   * @return true if lhs.last_name >= rhs.last_name. 
   * @return false if lhs.last_name < rhs.last_name. 
   */
  bool operator>=(Employee& e); 

  /**
   * @brief Overloading the == operator.
   * rhs: right hand side AND lhs: left hand side.
   * @param rhs Right hand side operant  
   * @return true if lhs.[anyfield] == rhs.[anyfield]. 
   * @return false if lhs.[anyfield] != rhs.[anyfield]. 
   */
  bool operator==(Employee& e);
  
  /**
   * @brief The assignment constructor.
   * rhs: right hand side AND lhs: left hand side.
   * @param rhs Right hand side operant  
   * @return The rhs object. 
   */
  Employee& operator=(const Employee& e);
};
#endif	/* ifdef */
