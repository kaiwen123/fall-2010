/** The main function for operations. 
 * @author Shumin Guo. guo.18@wright.edu
 * @version 1.0. 
 * @date 04/03/2012. 
 */ 
#include <cppconn/driver.h>
#include <cppconn/exception.h>
#include <cppconn/resultset.h>
#include <cppconn/statement.h>
#include "funcs.h"

using namespace std; 
//void retrieve_dbmetadata_and_print (Connection *dbcon);

int main(void) {
  try {
    sql::Driver *driver; 	// the database driver. 
    sql::Connection *conn;	// connection to the database. 
    sql::Statement *stmt; 	// sql statement.
    sql::ResultSet *res; 	// resources, such as query results, etc.

    // Create a connection. 
    driver = get_driver_instance(); 
    conn = driver->connect("tcp://localhost:3306", "root", ""); 

    // Connect to the CS701DB database; 
    conn->setSchema("CS701DB"); 

    // turn off the autocommit, and use commit statement explicitly. 
    conn->setAutoCommit(0); 
    cout << "\nDatabase connection\'s autocommit mode = " 
	 << conn->getAutoCommit() << endl; 

    // retrieve and display the database metadata. 
    // retrieve_dbmetadata_and_print(conn); 

    // Do operations.
    char choice;		// User command choice. 
    do {
      getCmd(choice);
      switch(tolower(choice)) {
      case 'i': { insertRecord(conn); break;}	// insert record. 
      case 'd': { deleteSpend(conn); break;}	// delete order record. 
      case 'm': { modifyIncome(conn); break;}	// modify order record. 
      case 'r': { retrieveRecord(conn); break;} // retrieve record.
      case 'q': exit(0); 
      default: break; 
      }
    } while(1);
    conn->close(); 
    delete res; 
    delete stmt; 
    delete conn; 
  } catch (SQLException &e) {
    cout << "ERROR: SQLException in " << __FILE__;
    cout << " (" << __func__<< ") on line " << __LINE__ << endl;
    cout << "ERROR: " << e.what();
    cout << " (MySQL error code: " << e.getErrorCode();
    cout << ", SQLState: " << e.getSQLState() << ")" << endl;

    if (e.getErrorCode() == 1047) {
      /*
	Error: 1047 SQLSTATE: 08S01 (ER_UNKNOWN_COM_ERROR)
	Message: Unknown command
      */
      cout << "\nYour server does not seem to support Prepared Statements at all. ";
      cout << "Perhaps MYSQL < 4.1?" << endl;
    }

    return EXIT_FAILURE;
  } catch (std::runtime_error &e) {

    cout << "ERROR: runtime_error in " << __FILE__;
    cout << " (" << __func__ << ") on line " << __LINE__ << endl;
    cout << "ERROR: " << e.what() << endl;

    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
