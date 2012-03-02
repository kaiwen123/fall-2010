#include "funcs.h"
#include <cppconn/driver.h>
#include <cppconn/exception.h>
#include <cppconn/resultset.h>
#include <cppconn/statement.h>

using namespace std; 

int main(void) {
  try {
    sql::Driver *driver; 
    sql::Connection *conn;
    sql::Statement *stmt; 
    sql::ResultSet *res; 

    // Create a connection. 
    driver = get_driver_instance(); 
    conn = driver->connect("tcp://localhost:3306", "ada", "825123"); 

    // Connect to the DB_PROJ database; 
    conn->setSchema("DB_PROJ"); 

    // turn off the autocommit. 
    conn->setAutoCommit(0); 
    cout << "\nDatabase connection\'s autocommit mode = " <<
      conn->getAutoCommit() << endl; 

    // retrieve and display the database metadata. 
    // retrieve_dbmetadata_and_print(conn); 

    // Do operations.
    char choice;			// User command choice. 
    do {
      getCmd(choice);
      switch(toLower(choice)) {
      case 'i': { insertRecord(conn); break;} // insert record. 
      case 'd': { deleteOrder(conn); break;} // delete order record. 
      case 'm': { modifyOrder(conn); break;}  // modify order record. 
      case 'r': { retrieveRecord(conn); break;} // retrieve record.
      case 'q': exit(0); 
      default: break; 
      }
    } while(1);
    conn->close(); 
    delete res; 
    delete stmt; 
    delete conn; 
  } catch (std::runtime_error &e) {
    cout << "ERROR: runtime_error in " << __FILE__;
    cout << " (" << __func__ << ") on line " << __LINE__ << endl;
    cout << "ERROR: " << e.what() << endl;
    return EXIT_FAILURE;
  }
}


   // Execute SQL statement and print result.
    // stmt = conn->createStatement(); 
    // stmt->execute("DELETE FROM Food;"); cout << "Old records deleted. " << endl; 
    // for(int i = 100; i < 200; i++){
    //   string statement = "INSERT INTO Food VALUES("+itoa(i)+", \"food\", \"Chinese\", 100)";
    //   stmt->execute(statement);
    // }
    // res = stmt->executeQuery("SELECT * FROM Food"); 

    // // print the result. 
    // cout << "Please see the Query result: \n"
    // 	 << "----------------------------" << endl;
    // while(res->next()) {
    //   cout << res->getInt(1) << " " 
    // 	   << res->getString(2) << " "
    // 	   << res->getString(3) << " "
    // 	   << res->getInt(4) << endl; 
    // }
