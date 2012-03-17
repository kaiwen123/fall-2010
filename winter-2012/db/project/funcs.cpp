/**
 * @file funcs.cpp File to implement the database operation functions.
 * @author Simon Guo. 
 * @date 04/03/2012.
 */
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <string>
#include "funcs.h"

using namespace std; 
// Prompt user to enter command 
// The command char will be returned through a reference char. 
void getCmd(char& choice) {
  cout << "\nWhich operation do you want to do?:" << endl
       << "(I/i)nsert new record." << endl
       << "(D/d)elete a record." << endl
       << "(R/r)etrieve record from database." << endl
       << "(M/m)odify record." << endl
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

// TODO.
// load data file into database.
// data files are dump of related databases, the fields 
// are seperated by tabs. 
bool dbLoad() {
  cout << "Loading data from file into database. \n" << endl; 
  fstream dbfile; 
  string line;			// line string. 
  // The bank account data. 
  dbfile.open("bankaccounts.dat", fstream::in);
  getline(dbfile, line, dbfile.widen('\n')); 

  dbfile.close();

  // income data. 
  dbfile.open("income.dat", fstream::in); 

  dbfile.close();

  // income data. 
  dbfile.open("spend.dat", fstream::in); 

  dbfile.close();

  // income data. 
  dbfile.open("budget.dat", fstream::in); 

  dbfile.close();

  // income data. 
  dbfile.open("debt.dat", fstream::in);

  dbfile.close();

  return true;
}

// handling of mysql database exceptions. 
void handleException(sql::SQLException e) {
  cout << "# ERROR: SQLException in " << __FILE__;
  cout << "# ERROR: " << e.what();
  cout << " (MySQL error code: " << e.getErrorCode();
  cout << ", SQLState: " << e.getSQLState() << " )" << endl;
  return; 
}

/**
 * @brief select table to operate on. 
 * @param head A string description. 
 * @return character.
 */
char selectTable(string &head) {
  char choice; 
  cout << head << endl
       << "(B)ank Account" << endl
       << "(I)ncome" << endl
       << "(S)pend" << endl
       << "(D)eposit" << endl
       << "(W)ithdrawal" << endl
       << endl << "Enter Choice: ";
  cin >> choice; 
  return tolower(choice); 
}

// insert operation.
bool insertRecord(sql::Connection *con) {
  string prompt = "Please select the table you want to insert into:"; 
  sql::Statement *stmt;
  sql::PreparedStatement *pstmt; 
  string stmtstr; 
  char tbl = selectTable(prompt); 
  switch(tbl) {
  case 'b': {
    string accntnm; double accntblnc; 
    string accntdate, accntmemo;
    cout << "Insert into bank account >>>> " << endl; 
    cout << "Account Name   : "; cin >> accntnm; 
    cout << "Account Balance: "; cin >> accntblnc; 
    cout << "Open Date      : "; cin >> accntdate; 
    cout << "Memo           : "; cin >> accntmemo; 

    try {
      stmt = con->createStatement(); 
      pstmt = con->prepareStatement("INSERT INTO BankAccount(AcntName,AcntBlnc,AcntDate,Memo)VALUES(?,?,?,?)"); 
      pstmt->setString(1, accntnm); 
      pstmt->setDouble(2, accntblnc); 
      pstmt->setString(3, accntdate);
      pstmt->setString(4, accntmemo); 
      pstmt->execute(); 
      // stmtstr = "INSERT INTO BankAccount(AccntId, AccntName, balance, opendate, memo) \
      // VALUES(\""+accntid+"\",\""+accntnm+"\",\""+accntblnc+"\",\"" +accntdate+"\",\""+accntmemo+"\")";
      // cout << stmtstr << endl; 
      // stmt->execute(stmtstr); 
      con->commit();
      cout << "Created account: " << accntnm << endl; 
      stmt->close(); 
    } catch(sql::SQLException &e) {
      handleException(e); 
    }
    break; 
  }
  case 'i': {
    string IncomeDate, description, saveToAccnt, amount;
    cout << "Insert into income table >>>>> " << endl; 
    cout << "Income Date       : "; cin >> IncomeDate; 
    cout << "Income Amount     : "; cin >> amount;  
    cout << "Income Description: "; cin >> description;
    try {
      stmt = con->createStatement(); 
      stmtstr = "INSERT INTO Income(IcmDate, IcmAmunt, Memo) \
      VALUES(\""+IncomeDate+"\",\""+amount+"\",\""+description+"\")";
      cout << stmtstr << endl; 
      stmt->execute(stmtstr); 
      con->commit(); 
      cout << "Income record is entered! " << endl; 
      stmt->close(); 
    } catch(sql::SQLException &e) {
      handleException(e); 
    }
    break; 
  }
  case 's': {
    string spenddate, location, memo;
    double amount; 
    cout << "Insert into Spend table >>>>  " << endl; 
    cout << "Date     : "; cin >> spenddate;
    cout << "Location : "; cin >> location; 
    cout << "Amount   : "; cin >> amount; 
    cout << "Memo     : "; cin >> memo; 
    try {
      stmt = con->createStatement(); 
      stmtstr = "INSERT INTO Spend (SpdDate, SpdLoc, SpdAmunt, Memo) \
        VALUES(\""+spenddate+"\",\""+location+"\",\""+itoa(amount)+"\",\""+memo+"\")";
      cout << stmtstr << endl; 
      stmt->execute(stmtstr); 
      // this will trigger the insertion of a withdrawal operation. 
      goto WD; 
      
      con->commit();
      cout << "Inserted spend record!" << endl; 
      stmt->close(); 
    } catch(sql::SQLException &e) {
      cout << "Exception happened spending ... withdrawal error. " << endl; 
      cout << "Rolling back ... " << endl;
      con->rollback(); 
      handleException(e); 
    }
    break;
  }
  case 'd': { 
    string dpto, dpfrom, dpdate, memo, amount;
    cout << "Insert into Deposit table >>>>  " << endl; 
    cout << "Deposit to Account :"; cin >> dpto; 
    cout << "Deposit from income:"; cin >> dpfrom; 
    cout << "Deposit Date       :"; cin >> dpdate;
    cout << "Amount             :"; cin >> amount; 
    cout << "Memo               :"; cin >> memo; 
    try {
      stmt = con->createStatement(); 
      stmtstr = "INSERT INTO Deposit (DpToAcnt, DpFrom, DpAmunt, DpDate, Memo) \
	VALUES(\""+dpto+"\",\""+dpfrom+"\",\""+amount+"\",\""+dpdate+"\",\""+memo+"\")";
      cout << stmtstr << endl; 
      stmt->execute(stmtstr); 
      con->commit();
      cout << "Inserted Deposit record!" << endl; 
      stmt->close(); 
    } catch(sql::SQLException &e) {
      handleException(e); 
    }
    break; 
  }
  case 'w': {
    WD: string wdfrom, wdfor, wddate, memo, amount;
    cout << "Insert into Withdrawal table >>>>  " << endl; 
    cout << "Withdrawal from Account :"; cin >> wdfrom; 
    cout << "Withdrawal for Expense  :"; cin >> wdfor; 
    cout << "Withdrawal Date         :"; cin >> wddate; 
    cout << "Withdrawal Amount       :"; cin >> amount; 
    cout << "Memo                    :"; cin >> memo; 
    try {
      stmt = con->createStatement(); 
      stmtstr = "INSERT INTO Withdrawal (WdFrom, WdFor, WdAmunt, WdDate, Memo)" \
	"VALUES(\""+wdfrom+"\",\""+wdfor+"\",\""+amount+"\",\""+wddate+"\",\""+memo+"\")";
      cout << stmtstr << endl; 
      stmt->execute(stmtstr); 
      throw sql::SQLException(); 

      con->commit();
      cout << "Inserted Withdrawal record!" << endl; 
      stmt->close(); 
    } catch(sql::SQLException &e) {
      cout << "Exception happened withdrawaling ... no enough fund..." << endl; 
      cout << "Rolling back ... " << endl;
      con->rollback();
      handleException(e); 
    }
    break; 
  }
  default: { cout << "Sorry, wrong input." << endl; }
  }
  delete stmt; 
  delete pstmt; 

  return true; 
}

/**
 * @brief delete order. 
 * @param con Database connection handle. 
 * @return true on success and false otherwise. 
 */ 
bool deleteSpend(sql::Connection *con) {
  cout << "Deleting Spend >>>> " << endl; 
  string spdid; 
  sql::Statement *stmt;
  sql::ResultSet *res; 
  try {
    cout << "Spend ID: "; cin >> spdid; 
    stmt = con->createStatement(); 
    res = stmt->executeQuery("SELECT * FROM Spend WHERE SpdId = \""+spdid+"\""); 
    cout << "\nSpend Details: " << endl; 
    cout << "+-------------------------------------+" << endl; 
    cout << "| Id | Date | Location| Amount | Memo |"<< endl; 
    cout << "+-------------------------------------+" << endl; 
    if(!res->next()) {
      cout << "Spend " << spdid << " doesn't exist." << endl; 
      return false; 
    }

    cout << res->getInt(1) << " | " 
	 << res->getString(2) << " | " 
	 << res->getString(3) << " | " 
	 << res->getDouble(4) << " | " 
	 << res->getString(5) << " | " << endl; 
    
    char dlt;
    cout << "Do you want to delete this spend record? (Y/N)"; 
    cin >> dlt; 
    if((dlt == 'Y') | (dlt == 'y')) {
      stmt = con->createStatement(); 
      stmt->execute("DELETE FROM Spend WHERE SpdId = \""+spdid+"\"");
      con->commit(); 
      cout << "Spend record " << spdid << " has been deleted successfully!" << endl;
    }
  } catch(sql::SQLException &e) {
    handleException(e);
  }
  return true; 
}

/**
 * @brief Modify the income record. 
 * @param con Database connection handle. 
 * @return true on success and false otherwise. 
 */ 
bool modifyIncome(sql::Connection *con) {
  string iid; 
  sql::ResultSet *res; 
  sql::Statement *stmt = con->createStatement(); 
  cout << "Income Id to modify: "; cin >> iid; 
  try {
    res = stmt->executeQuery("SELECT * FROM Income WHERE IcmId = \""+iid+"\""); 
    cout << "\nIncome details: " << endl; 
    if(!res->next()) {
      cout << "" << endl; 
      return false; 
    }
    cout << res->getInt(1) << " | " 
	 << res->getString(2) << " | " 
	 << res->getDouble(3) << " | " 
	 << res->getString(4) << " | " << endl; 
    
    char ans; 
    cout << "Do you want to update income amount? (Y/N) "; cin >> ans; 
    if((ans == 'Y') || (ans == 'y')) {
      string amunt; 
      cout << "Enter new amount: "; cin >> amunt; 
      stmt = con->createStatement(); 
      stmt->execute("UPDATE Income SET IcmAmunt = " + amunt + " WHERE IcmId = \"" + iid + "\""); 
      cout << "Updating income for record " << iid << " ... " << endl; 
      throw sql::SQLException(); 
      
      con->commit(); 
      cout << "Income amount was updated to " << amunt << endl; 
    }
  } catch(sql::SQLException &e) {
    cout << endl << "Exception happened ... !" << endl;
    cout << "Rolling back ... " << endl;  
    con->rollback(); 
    handleException(e);
  }
  return true; 
}

/**
 * @brief Retrieve Records from database. 
 * @param con Database connection handle. 
 * @return true on success and false otherwise. 
 */
bool retrieveRecord(sql::Connection *con) {
  string prompt = "which table to display:"; 
  sql::Statement *stmt = con->createStatement();
  sql::ResultSet *res = NULL; 
  char tbl = selectTable(prompt); 
  switch(tbl) {
  case 'b': {
    cout << "Existing Bank Accounts >>>> " << endl; 
    cout << "+----------------------------------------+" << endl; 
    cout << "| Id | Name | balance | open Date | memo |" << endl;
    cout << "+----------------------------------------+" << endl; 
    try {
      res = stmt->executeQuery("SELECT * FROM BankAccount;"); 
      // con->rollback();
      con->commit(); 
      while(res->next()) {
	cout << "| "<< res->getUInt(1) << " | "
	     << res->getString(2) << " | "
	     << res->getDouble(3) << " | "
	     << res->getString(4) << " | "
	     << res->getString(5) << endl; 
      }
      res->close();
      stmt->close(); 
    } catch(sql::SQLException &e) {
      handleException(e);
    }
    break; 
  }
  case 'i': {
    cout << "Income records >>>> " << endl; 
    res = stmt->executeQuery("SELECT * FROM Income;"); 
    cout << "+---------------------------+" << endl; 
    cout << "| Id | Date | Amount | Memo |" << endl;
    cout << "+---------------------------+" << endl; 
    while(res->next()) {
      cout << res->getUInt(1) << " | "
    	   << res->getString(2) << " | "
    	   << res->getDouble(3) << " | "
	   << res->getString(4) << " | " << endl; 
    }
    break; 
  }
  case 's': {    
    cout << "Spend Records: " << endl; 
    res = stmt->executeQuery("SELECT * FROM Spend;"); 
    cout << "+--------------------------------------+" << endl; 
    cout << "| Id | Date | Location | Amount | Memo |"<< endl; 
    cout << "+--------------------------------------+" << endl; 
    while(res->next()) {
      cout << res->getUInt(1) << " | " 
	   << res->getString(2) << " | " 
	   << res->getString(3) << " | " 
    	   << res->getDouble(4) << " | "
    	   << res->getString(5) << " | " << endl; 
    }
    break; 
  }

  case 'd': {    
    cout << "Deposit Records: " << endl; 
    res = stmt->executeQuery("SELECT * FROM Deposit;"); 
    cout << "+--------------------------------------------------------+" << endl; 
    cout << "| Id | To Account | From Income | Amount | Date | Memo |"<< endl; 
    cout << "+--------------------------------------------------------+" << endl; 
    while(res->next()) {
      cout << res->getUInt(1) << " | " 
	   << res->getUInt(2) << " | " 
	   << res->getUInt(3) << " | " 
    	   << res->getDouble(4) << " | "
	   << res->getString(5) << " | " 
    	   << res->getString(6) << " | " << endl; 
    }
    break; 
  }
  case 'w': {    
    cout << "Withdrawal Records: " << endl; 
    res = stmt->executeQuery("SELECT * FROM Withdrawal;"); 
    cout << "+--------------------------------------------------------+" << endl; 
    cout << "| Id | From Account | For Expense | Amount | Date | Memo |"<< endl; 
    cout << "+--------------------------------------------------------+" << endl; 
    while(res->next()) {
      cout << res->getUInt(1) << " | " 
	   << res->getUInt(2) << " | " 
	   << res->getUInt(3) << " | " 
	   << res->getDouble(4) << " | "
    	   << res->getString(5) << " | "
    	   << res->getString(6) << " | " << endl; 
    }
    break; 
  }

  default: { cout << "Sorry, wrong input." << endl; }
  }
  return true; 
}
