#include "funcs.h"

// Prompt user to enter command 
// The command char will be returned through a reference char. 
void getCmd(char& choice) {
  cout << "\nPlease Select Command From Following MENU:" << endl
       << "(I/i)nsert new record" << endl
       << "(D/d)elete record from database" << endl
       << "(R/r)etrieve record in database" << endl
       << "(Q/q)uit" << endl
       << endl << "Enter Choice: ";
  cin >> choice; 
}

/**
 * @brief transform an integer into a string object. 
 * @param integer The integer to be transformed. 
 * @return string representation of the integer. 
 */ 
string itoa(const int &integer){
  if (integer==0) return string("0");
  string a;
  int start, digits, piece;

  //count digits
  digits=0;
  piece=((integer<0)? 0-integer : integer);
  while( piece > 0 ) {
    piece-= (piece%10);
    piece/=10;
    digits++;
  }
  
  start=((integer<0)? 1 : 0);
  a.resize(digits+start,' ');
  if (integer<0) a[0]='-';
  
  piece=((integer<0)? 0-integer : integer);
  for(int i=0;  piece > 0; i++ ) {
    a[ digits+start-i-1] = (piece%10)+48;
    piece-= (piece%10);
    piece/=10;
  } 
  return a;
}

// table selection. 
char selectTable(string &head) {
  char choice; 
  cout << head << endl
       << "(C)ustomer." << endl
       << "(F)ood." << endl
       << "(O)rder." << endl
       << endl << "Enter Choice: ";
  cin >> choice; 
  return choice; 
}

// insert operation.
bool insertRecord(sql::Connection *con) {
  string prompt = "Please select the table you want to insert into:"; 
  char tbl = selectTable(prompt); 
  switch(tbl) {
  case 'c': {
    cout << "You have selected Customer table. " << endl; 
    doInsert(con, tbl); break; 
  }
  case 'f': {
    cout << "You have selected Food table." << endl; 
    doInsert(con, tbl); break; 
  }
  case 'r': {
    cout << "You have selected Order table. " << endl; 
    doInsert(con, tbl); break; 
  }
  default: { cout << "Sorry, wrong input." << endl; }
  }
  return true; 
}

bool doInsert(sql::Connection *con, char choice) {
  string first_name, last_name, address, phone; 
  string food_name, category; 
  int age, calorie;
  string stmtstr;  		// SQL statement string. 
  sql::Statement *stmt; 
  switch(choice) {
  case 'c': {
  cout << "Please provide customer information: " << endl; 
  cout << "Please enter first name(string): "; cin >> first_name; 
  cout << "Please enter last name(string): "; cin >> last_name;
  cout << "Please enter age(int): "; cin >> age;  
  cout << "Please enter address(string): "; cin >> address; 
  cout << "Please enter phone number(string): "; cin >> phone; 
  stmt = con->createStatement(); 
  stmtstr = "INSERT INTO Customer(CFname, CLname, CAddress, CAge, CPhone) \
  VALUES(\""+first_name+"\",\""+last_name+"\",\""+address+"\",\""\
    +itoa(age)+"\",\""+phone+"\")";
  cout << stmtstr << endl; 
  stmt->execute(stmtstr); 
  con->commit();
  break; 
  }
  case 'f': {
  cout << "Please enter food name(string): "; cin >> food_name; 
  cout << "Please enter food Category(string): "; cin >> category; 
  cout << "Please enter unit calorie(int): "; cin >> calorie;  
  stmt = con->createStatement(); 
  stmtstr = "INSERT INTO Food(FName, FCategory, FUcalorie) \
  VALUES(\""+food_name+"\",\""+category+"\",\""+itoa(calorie)+"\")"; 
  } 
  case 'o': {

  }
  }
  return true; 
}

/**
 * @brief delete operation. 
 * @param con Database connection handle. 
 * @return true on success and false otherwise. 
 */ 
bool deleteRecord(sql::Connection *con) {
  return true; 
}

/**
 * @brief delete operation. 
 * @param con Database connection handle. 
 * @return true on success and false otherwise. 
 */
bool retrieveRecord(sql::Connection *con) {
  return true; 
}

// convert a char from upper case to lower case.
char toLower(char c) {
  char ret; 
  if((c >= 'A') && (c <= 'Z')) ret = c + 'a' - 'A'; 
  else if((c >= 'a') && (c <= 'z')) ret = c;
  return ret; 
}
