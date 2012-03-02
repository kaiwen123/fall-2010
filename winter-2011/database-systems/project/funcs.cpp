#include "funcs.h"

// Prompt user to enter command 
// The command char will be returned through a reference char. 
void getCmd(char& choice) {
  cout << "\nPlease Select Command From Following MENU:" << endl
       << "(I/i)nsert new record." << endl
       << "(D/d)elete a order record." << endl
       << "(R/r)etrieve record in database." << endl
       << "(M/m)odify quantity of an order." << endl
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
  string cid, first_name, last_name, address, phone; 
  string food_name, category, stmtstr; 
  int age, calorie, quantity;
  string prompt = "Please select the table you want to insert into:"; 
  sql::Statement *stmt;
  char tbl = selectTable(prompt); 
  switch(tbl) {
  case 'c': {			// insert to Customer table. 
    cout << "You have selected Customer table. " << endl; 
    cout << "Please provide customer information: " << endl; 
    cout << "Please enter a customer id(string): "; cin >> cid; 
    cout << "Please enter first name(string): "; cin >> first_name; 
    cout << "Please enter last name(string): "; cin >> last_name;
    cout << "Please enter age(int): "; cin >> age;  
    cout << "Please enter address(string): "; cin >> address; 
    cout << "Please enter phone number(string): "; cin >> phone; 
    try {
      stmt = con->createStatement(); 
      stmtstr = "INSERT INTO Customer(CID, CFname, CLname, CAddress, CAge, CPhone) \
      VALUES(\""+cid+"\",\""+first_name+"\",\""+last_name+"\",\"" \
	+address+"\",\""+itoa(age)+"\",\""+phone+"\")";
      stmt->execute(stmtstr); 
      con->commit();
      cout << endl << first_name 
	   << ", thanks for registration. " << endl;  
      stmt->close(); 
    } catch(sql::SQLException &e) {
      cout << "# ERROR: SQLException in " << __FILE__;
      cout << "(" << __FUNCTION__ << ") on line " << __LINE__ << endl;
      cout << "# ERROR: " << e.what();
      cout << " (MySQL error code: " << e.getErrorCode();
      cout << ", SQLState: " << e.getSQLState() << " )" << endl;
    }
    break; 
  }
  case 'f': {			// insert to Food table. 
    cout << "You have selected Food table." << endl; 
    cout << "Please enter food name(string): "; cin >> food_name; 
    cout << "Please enter food Category(string): "; cin >> category; 
    cout << "Please enter unit calorie(int): "; cin >> calorie;  
    try {
      stmt = con->createStatement(); 
      stmtstr = "INSERT INTO Food(FName, FCategory, FUcalorie) \
      VALUES(\""+food_name+"\",\""+category+"\",\""+itoa(calorie)+"\")";
      stmt->execute(stmtstr); 
      con->commit(); 
      cout << food_name << " is successfully registrated! " << endl; 
      stmt->close(); 
    } catch(sql::SQLException &e) {
      cout << "# ERROR: SQLException in " << __FILE__;
      cout << "(" << __FUNCTION__ << ") on line " << __LINE__ << endl;
      cout << "# ERROR: " << e.what();
      cout << " (MySQL error code: " << e.getErrorCode();
      cout << ", SQLState: " << e.getSQLState() << " )" << endl;
    }
    break; 
  }
  case 'o': {			// insert to FOrder table. 
    cout << "You have selected Order table. " << endl; 
    cout << "Please enter customer id(string): "; cin >> cid;   
    cout << "Please enter food name(string) you want to order: "; cin >> food_name; 
    cout << "Please enter quantity(int): "; cin >> quantity;  
    try {
      stmt = con->createStatement(); 
      stmtstr = "INSERT INTO FOrder (OCID, OFName, OQuant)\
  VALUES(\""+cid+"\",\""+food_name+"\",\""+itoa(quantity)+"\")";
      stmt->execute(stmtstr); 
      con->commit();
      cout << "Order successfully placed, thanks for your business!" << endl; 
      stmt->close(); 
    } catch(sql::SQLException &e) {
      cout << "# ERROR: SQLException in " << __FILE__;
      cout << "(" << __FUNCTION__ << ") on line " << __LINE__ << endl;
      cout << "# ERROR: " << e.what();
      cout << " (MySQL error code: " << e.getErrorCode();
      cout << ", SQLState: " << e.getSQLState() << " )" << endl; 
    }
    break;
  }
  default: { cout << "Sorry, wrong input." << endl; }
  }
  return true; 
}

/**
 * @brief delete order. 
 * @param con Database connection handle. 
 * @return true on success and false otherwise. 
 */ 
bool deleteOrder(sql::Connection *con) {
  cout << "Deleting food order. " << endl; 
  string cid; 
  int id; 
  sql::Statement *stmt;
  sql::ResultSet *res; 
  try {
    cout << "Please enter your customer ID: "; cin >> cid; 
    stmt = con->createStatement(); 
    res = stmt->executeQuery("SELECT * FROM FOrder WHERE OCID = \""+cid+"\""); 
    cout << "++++++++++++++++++++++++++++++++++++++ " << endl; 
    cout << "\nYou have following orders: " << endl; 
    while(res->next()) {
      cout << res->getInt(1) << " | " 
	   << res->getString(2) << " | " 
	   << res->getString(3) << " | " 
    	   << res->getString(4) << " | "
    	   << res->getInt(5) << " | " << endl; 
    }
    cout << "Please enter the order id you want to delete: "; cin >> id; 
    stmt = con->createStatement(); 
    res = stmt->executeQuery("SELECT * FROM FOrder WHERE OID = \"" + itoa(id)+ \
		       "\" AND OCID = \"" +cid+"\""); 
    if(!(res->next())) {
      cout << "The order you want to delete doesn't exist." << endl; 
    } else {
      stmt->execute("DELETE FROM FOrder WHERE OID = \"" + itoa(id)+	\
		    "\" AND OCID = \"" +cid+"\"");
      con->commit(); 
      cout << "Order " << id << " successfully deleted!" << endl;
    }
  } catch(sql::SQLException &e) {
      cout << "# ERROR: SQLException in " << __FILE__;
      cout << "(" << __FUNCTION__ << ") on line " << __LINE__ << endl;
      cout << "# ERROR: " << e.what();
      cout << " (MySQL error code: " << e.getErrorCode();
      cout << ", SQLState: " << e.getSQLState() << " )" << endl; 
  }
  return true; 
}

/**
 * @brief modify order. The order record will be modified. 
 * @param con Database connection handle. 
 * @return true on success and false otherwise. 
 */ 
bool modifyOrder(sql::Connection *con) {
  cout << "Modifying food order." << endl; 
  int id, quant; 
  string cid; 
  sql::ResultSet *res; 
  sql::Statement *stmt = con->createStatement(); 
  cout << "Please enter your customer ID: "; cin >> cid; 
  try {
    res = stmt->executeQuery("SELECT * FROM FOrder WHERE OCID = \""+cid+"\""); 
    cout << "++++++++++++++++++++++++++++++++++++++ " << endl; 
    cout << "\nYou have following orders: " << endl; 
    while(res->next()) {
      cout << res->getInt(1) << " | " 
	   << res->getString(2) << " | " 
	   << res->getString(3) << " | " 
    	   << res->getString(4) << " | "
    	   << res->getInt(5) << " | " << endl; 
    }
    cout << "Please enter the order id you want to change: "; cin >> id; 
    stmt = con->createStatement(); 
    res = stmt->executeQuery("SELECT OQuant FROM FOrder WHERE OID = \"" + itoa(id)+ \
			     "\" AND OCID = \"" +cid+"\""); 
    if(!(res->next())) {
      cout << "The order you want to update doesn't exist." << endl; 
    } else {
      cout << "Your current order quantity is: " << res->getInt(1) << endl; 
      cout << "Please enter your new quantity: "; cin >> quant; 
      cout << "Updating..."; 
      stmt->execute("UPDATE FOrder SET OQuant = "+itoa(quant)); 
      con->commit(); 
      cout << "Order " << id << " has been changed to " << quant << endl; 
      cout << "Thanks for your business!" << endl; 
    }
  } catch(sql::SQLException &e) {
    cout << "# ERROR: SQLException in " << __FILE__;
    cout << "(" << __FUNCTION__ << ") on line " << __LINE__ << endl;
    cout << "# ERROR: " << e.what();
    cout << " (MySQL error code: " << e.getErrorCode();
    cout << ", SQLState: " << e.getSQLState() << " )" << endl; 
  }
  return true; 
}

/**
 * @brief delete operation. 
 * @param con Database connection handle. 
 * @return true on success and false otherwise. 
 */
bool retrieveRecord(sql::Connection *con) {
  string prompt = "Please select the table you want to retrieve:"; 
  sql::Statement *stmt = con->createStatement();
  sql::ResultSet *res = NULL; 
  char tbl = selectTable(prompt); 
  switch(tbl) {
  case 'c': {
    cout << "Currently registered Customers. " << endl; 
    cout << "-------------------------------------------" << endl; 
    cout << "id | fname | lname | address | age | phone " << endl;
    cout << "-------------------------------------------" << endl; 
    try {
      res = stmt->executeQuery("SELECT * FROM Customer;"); 
      while(res->next()) {
	cout << res->getString(1) << " | "
	     << res->getString(2) << " | "
	     << res->getString(3) << " | "
	     << res->getString(4) << " | "
	     << res->getInt(5) << " | "
	     << res->getString(6) << endl; 
      }
      res->close();
      stmt->close(); 
    } catch(sql::SQLException &e) {
      cout << "# ERROR: SQLException in " << __FILE__;
      cout << "(" << __FUNCTION__ << ") on line " << __LINE__ << endl;
      cout << "# ERROR: " << e.what();
      cout << " (MySQL error code: " << e.getErrorCode();
      cout << ", SQLState: " << e.getSQLState() << " )" << endl; 
    }
    break; 
  }
  case 'f': {
    
    cout << "Currently registered Food. " << endl; 
    res = stmt->executeQuery("SELECT * FROM Food;"); 
    cout << "----------------------------------------" << endl; 
    cout << "| name | category | calorie " << endl;
    cout << "----------------------------------------" << endl; 
    while(res->next()) {
      cout << res->getString(1) << " | "
    	   << res->getString(2) << " | "
    	   << res->getInt(3) << " | " << endl; 
    }
    break; 
  }
  case 'o': {    
    cout << "Currently placed Orders. " << endl; 
    res = stmt->executeQuery("SELECT * FROM FOrder;"); 
    cout << "---------------------------------------------------" << endl; 
    cout << "id | customer | food |       time       | quantity "<< endl; 
    cout << "---------------------------------------------------" << endl; 
    while(res->next()) {
      cout << res->getInt(1) << " | " 
	   << res->getString(2) << " | " 
	   << res->getString(3) << " | " 
    	   << res->getString(4) << " | "
    	   << res->getInt(5) << " | " << endl; 
    }
    break; 
  }
  default: { cout << "Sorry, wrong input." << endl; }
  }
  return true; 
}

// convert a char from upper case to lower case.
char toLower(char c) {
  char ret; 
  if((c >= 'A') && (c <= 'Z')) ret = c + 'a' - 'A'; 
  else if((c >= 'a') && (c <= 'z')) ret = c;
  return ret; 
}
