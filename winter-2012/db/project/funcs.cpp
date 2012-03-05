/**
 * @file funcs.cpp File to implement the database operation functions.
 * @author Simon Guo. 
 * @date 04/03/2012.
 */
#include "funcs.h"

// Prompt user to enter command 
// The command char will be returned through a reference char. 
void getCmd(char& choice) {
  cout << "\nWhich operation do you want to do?:" << endl
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

// TODO.
// load data file into database.
// data files are dump of related databases, the fields 
// are seperated by tabs. 
bool dbLoad() {
  cout << "Loading data into database. \n" << endl; 
  FILE *dbfile; 
  string line;			// line string. 
  // The bank account data. 
  dbfile = fopen("bankaccounts.dat", "r");
  getline(dbfile, str); 

  fclose(dbfile);

  // income data. 
  dbfile = fopen("income.dat", "r"); 

  fclose(dbfile); 

  // income data. 
  dbfile = fopen("spend.dat", "r"); 

  fclose(dbfile); 

  // income data. 
  dbfile = fopen("budget.dat", "r"); 

  fclose(dbfile); 

  // income data. 
  dbfile = fopen("debt.dat", "r"); 

  fclose(dbfile); 
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
       << "B(u)dget" << endl
       << "(D)ebt" << endl
       << endl << "Enter Choice: ";
  cin >> choice; 
  return tolower(choice); 
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
  case 'b': {
    string accntid, accntnm, accntblnc; 
    string accntdate, accntmemo;
    cout << "Insert into bank account >>>> " << endl; 
    cout << "Account ID     : "; cin >> accntid;  
    cout << "Account Name   : "; cin >> accntnm; 
    cout << "Account Balance: "; cin >> accntblnc; 
    cout << "Open Date      : "; cin >> accntdate; 
    cout << "Memo           : "; cin >> accntmemo; 

    try {
      stmt = con->createStatement(); 
      stmtstr = "INSERT INTO BankAccount(AccntId, AccntName, balance, opendate, accntmemo) \
      VALUES(\""+accntid+"\",\""+accntnm+"\",\""+accntblnc+"\",\"" \
	+accntdate+"\",\""+accntmemo+"\")";
      stmt->execute(stmtstr); 
      con->commit();
      stmt->close(); 
    } catch(sql::SQLException &e) {
      handleException(e); 
    }
    break; 
  }
  case 'i': {
    string incid, incdate, incamunt, incdesc, saveto; 
    cout << "Insert into income table >>>>> " << endl; 
    cout << "Income ID         : "; cin >> incid; 
    cout << "Income Date       : "; cin >> incdate; 
    cout << "Income Amount     : "; cin >> incamunt;  
    cout << "Income Description: "; cin >> incdesc;
    cout << "Save to Account   : "; cin >> saveto; 
    try {
      stmt = con->createStatement(); 
      stmtstr = "INSERT INTO Income(IncomeId, IncomeDate, amount, description, saveToAccnt) \
      VALUES(\""+IncomeId+"\",\""+IncomeDate+"\",\""+amount+"\"","\"+description+"\",\""+saveToAccnt\")";
      stmt->execute(stmtstr); 
      con->commit(); 
      cout << incid << " is entered! " << endl; 
      stmt->close(); 
    } catch(sql::SQLException &e) {
      handleException(e); 
    }
    break; 
  }
  case 'o': {
    string spenddate, location, reason, fromaccnt, memo;
    cout << "Insert into Spend table >>>>  " << endl; 
    cout << "Date     : "; cin >> spenddate;
    cout << "Location : "; cin >> location; 
    cout << "Reason   :"; cin >> reason; 
    cout << "From Acnt:"; cin >> fromaccnt; 
    cout << "Memo     :"; cin >> memo; 
    try {
      stmt = con->createStatement(); 
      stmtstr = "INSERT INTO Spend (SpendDate, location, forwhat, chargefromAccnt)\
  VALUES(\""+spendate+"\",\""+location+"\",\""+reason+"\","\"+chargefromAccnt+"\",\""+memo"\")";
      stmt->execute(stmtstr); 
      con->commit();
      cout << "Inserted spend record!" << endl; 
      stmt->close(); 
    } catch(sql::SQLException &e) {
      handleException(e); 
    }
    break;
  }
  case 'u' {
    double amount; 
    string bgtid, bgtdate, reason, fromaccnt, memo;
    cout << "Insert into Budget table >>>>  " << endl; 
    cout << "Budget Id :"; cin >> bgtid; 
    cout << "Reason    :"; cin >> reason; 
    cout << "Date      :"; cin >> bgtdate;
    cout << "From Acnt :"; cin >> fromaccnt; 
    cout << "Amount    :"; cin >> amount; 
    cout << "Memo     :"; cin >> memo; 
    try {
      stmt = con->createStatement(); 
      stmtstr = "INSERT INTO Budget (BudgetId, BudgetFor, CreateDate, BudgetFromAccnt, Amount, Memo)\
  VALUES(\""+bgtid+"\",\""+reason+"\",\""+bgtdate+"\","\"+fromaccnt+"\",\""+amount"\",\""+memo"\")";
      stmt->execute(stmtstr); 
      con->commit();
      cout << "Inserted Budget record!" << endl; 
      stmt->close(); 
    } catch(sql::SQLException &e) {
      handleException(e); 
    }
    break; 
  }
  case 'd' {
    double amount;
    string debtid, debttype, debtstart, debtend, memo;
    cout << "Insert into Debt table >>>>  " << endl; 
    cout << "Debt ID         :"; cin >> debtid;
    cout << "Debt Type       :"; cin >> debttype; 
    cout << "Debt Start Date :"; cin >> debtstart; 
    cout << "Debt End Date   :"; cin >> debtend; 
    cout << "Debt Amount     :"; cin >> amount; 
    cout << "Memo            :"; cin >> memo; 
    try {
      stmt = con->createStatement(); 
      stmtstr = "INSERT INTO Debt (DebtId, DebtType, DebtStartDate, DebtEndDate, DebtAmount, Memo)\
  VALUES(\""+debtid+"\",\""+debttype+"\",\""+debtstart+"\","\"+debtend+"\",\""+amount+"\",\""+memo"\")";
      stmt->execute(stmtstr); 
      con->commit();
      cout << "Inserted debt record!" << endl; 
      stmt->close(); 
    } catch(sql::SQLException &e) {
      handleException(e); 
    }
    break; 
  }
  default: { cout << "Sorry, wrong input." << endl; }
  }
  return true; 
}

void handleException(sql::Exception e) {
  cout << "# ERROR: SQLException in " << __FILE__;
  cout << "# ERROR: " << e.what();
  cout << " (MySQL error code: " << e.getErrorCode();
  cout << ", SQLState: " << e.getSQLState() << " )" << endl;
  return; 
}
/**
 * @brief delete order. 
 * @param con Database connection handle. 
 * @return true on success and false otherwise. 
 */ 
bool deleteBudget(sql::Connection *con) {
  cout << "Deleting Budget >>>> " << endl; 
  string bid; 
  int id; 
  sql::Statement *stmt;
  sql::ResultSet *res; 
  try {
    cout << "Budget ID: "; cin >> bid; 
    stmt = con->createStatement(); 
    res = stmt->executeQuery("SELECT * FROM Budget WHERE BudgetId = \""+bid+"\""); 
    cout << "\nBudget Details: " << endl; 
    cout << "+--------------------------------------------------------+" << endl; 
    cout << "| Id | Reason | Create Date | From Accnt | Amount | Memo |"<< endl; 
    cout << "+--------------------------------------------------------+" << endl; 
    if(!res->next()) {
      cout << "Budget " << bid << " doesn't exist." << endl; 
      return false; 
    }

    cout << res->getString(1) << " | " 
	 << res->getString(2) << " | " 
	 << res->getDate(3) << " | " 
	 << res->getString(4) << " | " 
	 << res->getDouble(5) << " | "
	 << res->getString(6) << " | " << endl; 
    
    char dlt;
    cout << "Do you want to delete this budget? (Y/N)"; 
    cin >> dlt; 
    if(dlt == 'Y' | dlt == 'y')
      stmt = con->createStatement(); 
      stmt->execute("DELETE FROM Budget WHERE BudgetId = \"" + bid + "\"");
      con->commit(); 
      cout << "Budget " << bid << " has been deleted successfully!" << endl;
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
  int id, quant; 
  string iid; 
  sql::ResultSet *res; 
  sql::Statement *stmt = con->createStatement(); 
  cout << "Income Id to modify: "; cin >> iid; 
  try {
    res = stmt->executeQuery("SELECT * FROM FOrder WHERE OCID = \""+iid+"\""); 
    cout << "\nIncome details: " << endl; 
    if(!res->next()) {
      cout << "" << endl; 
      return false; 
    }
    cout << res->getInt(1) << " | " 
	 << res->getString(2) << " | " 
	 << res->getString(3) << " | " 
	 << res->getString(4) << " | "
	 << res->getInt(5) << " | " << endl; 
    
    char ans; 
    cout << "Do you want to update income amount? (Y/N) "; cin >> ans; 
    if(ans == 'Y' || ans == 'y') {
      double amunt; 
      cout << "Enter new amount: "; cin >> amunt; 
      stmt = con->createStatement(); 
      stmt->execute("UPDATE Income SET amount = " + amunt); 
      con->commit(); 
      cout << "Income amount was updated to " << amunt << endl; 
    }
  } catch(sql::SQLException &e) {
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
    cout << "| Id | Name | open Date | balance | memo |" << endl;
    cout << "+----------------------------------------+" << endl; 
    try {
      res = stmt->executeQuery("SELECT * FROM BankAccount;"); 
      while(res->next()) {
	cout << res->getString(1) << " | "
	     << res->getString(2) << " | "
	     << res->getString(3) << " | "
	     << res->getDouble(4) << " | "
	     << res->getDate(5) << endl; 
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
    cout << "+--------------------------------------------+" << endl; 
    cout << "| Id | Date | Amount | Description | Save To |" << endl;
    cout << "+--------------------------------------------+" << endl; 
    while(res->next()) {
      cout << res->getString(1) << " | "
    	   << res->getDate(2) << " | "
    	   << res->getDouble(3) << " | "
	   << res->getString(4) << " | "
	   << res->getString(5) << " | " << endl; 
    }
    break; 
  }
  case 's': {    
    cout << "Spend Records: " << endl; 
    res = stmt->executeQuery("SELECT * FROM Spend;"); 
    cout << "+----------------------------------------------+" << endl; 
    cout << "| Date | Location | Reason | From Accnt | Memo |"<< endl; 
    cout << "+----------------------------------------------+" << endl; 
    while(res->next()) {
      cout << res->getDate(1) << " | " 
	   << res->getString(2) << " | " 
	   << res->getString(3) << " | " 
    	   << res->getString(4) << " | "
    	   << res->getString(5) << " | " << endl; 
    }
    break; 
  }

  case 'u': {    
    cout << "Budget Records: " << endl; 
    res = stmt->executeQuery("SELECT * FROM Budget;"); 
    cout << "+--------------------------------------------------------+" << endl; 
    cout << "| Id | Reason | Create Date | From Accnt | Amount | Memo |"<< endl; 
    cout << "+--------------------------------------------------------+" << endl; 
    while(res->next()) {
      cout << res->getString(1) << " | " 
	   << res->getString(2) << " | " 
	   << res->getDate(3) << " | " 
	   << res->getString(4) << " | " 
    	   << res->getDouble(5) << " | "
    	   << res->getString(6) << " | " << endl; 
    }
    break; 
  }
  case 'd': {    
    cout << "Debt Records: " << endl; 
    res = stmt->executeQuery("SELECT * FROM Debt;"); 
    cout << "+-----------------------------------------+" << endl; 
    cout << "| Id | Type | Start | End | Amount | Memo |"<< endl; 
    cout << "+-----------------------------------------+" << endl; 
    while(res->next()) {
      cout << res->getString(1) << " | " 
	   << res->getString(2) << " | " 
	   << res->getDate(3) << " | " 
    	   << res->getDate(4) << " | "
	   << res->getDouble(5) << " | "
    	   << res->getString(5) << " | " << endl; 
    }
    break; 
  }

  default: { cout << "Sorry, wrong input." << endl; }
  }
  return true; 
}
