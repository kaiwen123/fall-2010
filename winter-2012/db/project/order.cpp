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
    conn = driver->connect("tcp://localhost:3306", "ada", "825123"); 

    // Connect to the DB_PROJ database; 
    conn->setSchema("DB_PROJ"); 

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
      case 'd': { deleteOrder(conn); break;}	// delete order record. 
      case 'm': { modifyOrder(conn); break;}	// modify order record. 
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

// functions to get informaiton about database. 
// void retrieve_dbmetadata_and_print (Connection *dbcon) {

//   if (dbcon -> isClosed()) {
//     throw runtime_error("DatabaseMetaData FAILURE - database connection closed");
//   }

//   cout << "\nDatabase Metadata" << endl;
//   cout << "-----------------" << endl;

//   cout << boolalpha;

//   /* The following commented statement won't work with Connector/C++
//      1.0.5 and later */
//   //auto_ptr < DatabaseMetaData > dbcon_meta (dbcon -> getMetaData());

//   DatabaseMetaData *dbcon_meta = dbcon -> getMetaData();

//   cout << "Database Product Name: " << dbcon_meta -> getDatabaseProductName() << endl;
//   cout << "Database Product Version: " << dbcon_meta -> getDatabaseProductVersion() << endl;
//   cout << "Database User Name: " << dbcon_meta -> getUserName() 
//        << endl << endl;

//   cout << "Driver name: " << dbcon_meta -> getDriverName() << endl;
//   cout << "Driver version: " << dbcon_meta -> getDriverVersion() <<
//     endl << endl;

//   cout << "Database in Read-Only Mode?: " << dbcon_meta -> isReadOnly() << endl;
//   cout << "Supports Transactions?: " << dbcon_meta -> supportsTransactions() << endl;
//   cout << "Supports DML Transactions only?: " << dbcon_meta -> supportsDataManipulationTransactionsOnly() << endl;
//   cout << "Supports Batch Updates?: " << dbcon_meta -> supportsBatchUpdates() << endl;
//   cout << "Supports Outer Joins?: " << dbcon_meta -> supportsOuterJoins() << endl;
//   cout << "Supports Multiple Transactions?: " << dbcon_meta -> supportsMultipleTransactions() << endl;
//   cout << "Supports Named Parameters?: " << dbcon_meta -> supportsNamedParameters() << endl;
//   cout << "Supports Statement Pooling?: " << dbcon_meta -> supportsStatementPooling() << endl;
//   cout << "Supports Stored Procedures?: " << dbcon_meta -> supportsStoredProcedures() << endl;
//   cout << "Supports Union?: " << dbcon_meta -> supportsUnion() << endl << endl;

//   cout << "Maximum Connections: " << dbcon_meta -> getMaxConnections() << endl;
//   cout << "Maximum Columns per Table: " << dbcon_meta -> getMaxColumnsInTable() << endl;
//   cout << "Maximum Columns per Index: " << dbcon_meta -> getMaxColumnsInIndex() << endl;
//   cout << "Maximum Row Size per Table: " << dbcon_meta -> getMaxRowSize() << " bytes" << endl;

//   cout << "\nDatabase schemas: " << endl;

//   auto_ptr < ResultSet > rs ( dbcon_meta -> getSchemas());

//   cout << "\nTotal number of schemas = " << rs -> rowsCount() << endl;
//   cout << endl;

//   int row = 1;

//   while (rs -> next()) {
//     cout << "\t" << row << ". " << rs -> getString("TABLE_SCHEM") <<
//       endl;
//     ++row;
//   } // while

//   cout << endl << endl;

// } // retrieve_dbmetadata_and_print()


// void retrieve_rsmetadata_and_print (ResultSet *rs) {

//   if (rs -> rowsCount() == 0) {
//     throw runtime_error("ResultSetMetaData FAILURE - no records in the result set");
//   }

//   cout << "ResultSet Metadata" << endl;
//   cout << "------------------" << endl;

//   /* The following commented statement won't work with Connector/C++
//      1.0.5 and later */
//   //auto_ptr < ResultSetMetaData > res_meta ( rs -> getMetaData() );

//   ResultSetMetaData *res_meta = rs -> getMetaData();

//   int numcols = res_meta -> getColumnCount();
//   cout << "\nNumber of columns in the result set = " << numcols <<
//     endl << endl;

//   cout.width(20);
//   cout << "Column Name/Label";
//   cout.width(20);
//   cout << "Column Type";
//   cout.width(20);
//   cout << "Column Size" << endl;

//   for (int i = 0; i < numcols; ++i) {
//     cout.width(20);
//     cout << res_meta -> getColumnLabel (i+1);
//     cout.width(20); 
//     cout << res_meta -> getColumnTypeName (i+1);
//     cout.width(20); 
//     cout << res_meta -> getColumnDisplaySize (i+1) << endl << endl;
//   }

//   cout << "\nColumn \"" << res_meta -> getColumnLabel(1);
//   cout << "\" belongs to the Table: \"" << res_meta -> getTableName(1);
//   cout << "\" which belongs to the Schema: \"" << res_meta -> getSchemaName(1) << "\"" << endl << endl;

// } // retrieve_rsmetadata_and_print()
