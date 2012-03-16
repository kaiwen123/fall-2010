#ifndef _FUNCS_H_
#define _FUNCS_H_ 

#include <iostream>
#include <stdlib.h>
#include <cppconn/driver.h>
#include <cppconn/exception.h>
#include <cppconn/resultset.h>
#include <cppconn/statement.h>
#include <cppconn/prepared_statement.h>

using namespace std; 
using namespace sql;

string itoa(const int &integer); 
void getCmd(char& choice);
bool insertRecord(sql::Connection *con); 
bool deleteSpend(sql::Connection *con); 
bool modifyIncome(sql::Connection *con); 
bool retrieveRecord(sql::Connection *con); 
bool doInsert(sql::Connection *con, char choice);
#endif 
