// main.cpp
//   Main program for testing hash table functions.
//   M. Raymer, 2/2007
//   Updated: D. C. Wlodarski, 5/2010
// -------------------------------------------------------------------
#include "ClosedHash.h"
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <list>
#include <string>

using namespace std;

#define MAXID 1000000     // Highest key/ID value allowed

//-----------*
// randomID /
//------------------------------------------------------------------
// Generate a random id for inserting into the hash
//   Since rand() returns a number between 0 and 32767 on our
//   UNIX system, we have to do this in a bit of a roundabout
//   way, by generating the high order bits and then the low
//   order ones.
//------------------------------------------------------------------
int randomID() {
  unsigned returnval;
  returnval = rand();  		   // Generate 16 bits of random data
  returnval = returnval << 16;   // Move to the upper 16 bits
  returnval ^= rand();           // Generate lower 16 bits
  return (returnval % MAXID) + 1;
}

// NOTE:
//   An interesting way to get a non-uniformly distributed random id would be
//   to add the result of 32 rand() calls together!
// ------------------------------------------------------------------------------

//---------------*
// menu renderer /
//-----------------------------------------------------------------
//  Render the CL interface to the terminal.
//-----------------------------------------------------------------
void render_menu() {
  // Prompt the user for an action:
  cout << "\n    -------------- MENU --------------\n";
  cout << "(A) Insert random ID's into the hash\n";
  cout << "(B) Delete random ID's from the hash\n";
  cout << "(C) Search the hash for random IDs\n";
  cout << "(D) Insert a specific ID into the hash\n";
  cout << "(E) Delete a specific ID from the hash\n";
  cout << "(F) Search the hash for a specific ID\n";
  cout << "(Q) Quit" << endl;
}

//--------------------------*
// Random inserter for hash /
//-----------------------------------------------------------------
//  Beats the hash down with par_count number of random IDs,
//  returning the total successfully inserted, storing the
//  successful IDs in par_listOfIDs, and the number of collisions
//  in par_collisions.
//-----------------------------------------------------------------
int insertRands(ClosedHash & par_hash, unsigned par_count, list<int> & par_listOfIDs, int & par_collisions) {

  int ID;				// ID to insert/delete/find
  int collisions = 0;
  unsigned insertedCtr = 0;

  par_collisions = 0;

  // Keep generating and inserting random ID values until
  // 'par_count' ID's have been successfully inserted into
  // the hash or the hash fills.  Keep track of the total
  // number of collisions during inserts.
  ofstream fdata("insert-collision-double.txt");
  while (insertedCtr < par_count && !(par_hash.full())) {
    ID = randomID();
    if (par_hash.insert(ID, collisions)) {
      par_listOfIDs.push_back(ID);
      insertedCtr++;
      par_collisions += collisions;
      fdata << par_hash.alpha() << "\t" << collisions << endl; 
    }
  }
  fdata.close();

  return insertedCtr;
}

//--------------------------------------*
// delete random count of IDs from hash /
//-----------------------------------------------------------------
// Delete the last 'count' IDs in par_listOfIDs from the hash.
//-----------------------------------------------------------------
int deleteRandCountIDs(ClosedHash & par_hash, unsigned par_count, list<int> & par_listOfIDs) {

  int deleteCnt = 0;

  if (par_count <= 0) {
    cerr << "Unable to delete " << par_count << " records." << endl;
  }
  else if (par_count > par_listOfIDs.size()) {
    cerr << "Unable to delete " << par_count << " records," << endl;
    cerr << " There are only " << par_listOfIDs.size();
    cerr << " random IDs in the hash." << endl;
  }

  // Delete the first 'par_count' IDs from both the par_listOfIDs
  // and the hash.  Report any errors to the user.
  else {
    deleteCnt = 0;
    for (unsigned i = 0; i < par_count; i++) {
      if (par_hash.remove(par_listOfIDs.back())) {
	deleteCnt++;
      }
      else {
	cerr << "ERROR:  unable to delete ID: ";
	cerr << par_listOfIDs.back() << endl;
      }
      par_listOfIDs.pop_back();
    }
  }

  return deleteCnt;

}

//----------------------------------------*
// search for random count of IDs in hash /
//-----------------------------------------------------------------
// Search for the last 'count' IDs in par_listOfIDs in the hash.
//-----------------------------------------------------------------
int searchHash(ClosedHash & par_hash, unsigned par_count, list<int> & par_listOfIDs, int & par_probes) {

  list<int>::const_iterator IDlistPosition;
  int probes = 0, foundCnt = 0;

  if (par_count <= 0) {
    cerr << "Unable to perform " << par_count << " searches." << endl;
  }
  else if (par_count > par_listOfIDs.size()) {
    cerr << "Unable to search for " << par_count << " records," << endl;
    cerr << " There are only " << par_listOfIDs.size();
    cerr << " random IDs in the hash." << endl;
  }

  // Search for the *LAST* 'par_count' IDs in the par_listOfIDs.  Report
  // the number of probes.
  else {
    par_probes = 0;
    IDlistPosition = par_listOfIDs.end();
    for (unsigned i = 0; i < par_count; i++) {
      IDlistPosition--;
      if (par_hash.find(*IDlistPosition, probes)) {
	foundCnt++;
	par_probes += probes;
      }
      else {
	cerr << "ERROR:  unable to find ID: ";
	cerr << (*IDlistPosition) << endl;
      }
    }
  }
  return foundCnt;
}

//---------------*
// main program /
//---------------*
int main() {

  ClosedHash theHash(32768);

  // LOCAL VARIABLES
  string response = ""; // User's response to prompts
  int ID;
  unsigned count;
  int inserted;                      // The following variables are...
  int probes;                        // ...for keeping track of the number...
  int totalCollisions, totalProbes;  // ...of collisons, probes...
  int deleted, found;                // ...and records found, etc.
  bool success;                      // Success flag for direct method calls

  // The IDlist keeps track of all the random ID's that have been
  // inserted into the hash.  That way, we can search for and delete
  // random ID's that we have already inserted into the table.
  list<int> IDlist;

  srand(static_cast<unsigned>(time(0)));  // Initialize the random number generator

  // MAIN LOOP
  //   Prompt the user for an action and then perform that action,
  //   until the user chooses to quit.
  while (response[0] != 'Q') {

    render_menu();

    cout << endl << "Response ==> ";
    cout.flush();
    getline(cin, response);

    switch (response[0]) {

      //--------------- Insert random ID's ----------------------------
    case 'A':
    case 'a':

      // Determine how many ID's to insert
      cout << endl << "How many random ID's to insert? ==> ";
      cout.flush();
      getline(cin, response);
      count = strtol(response.c_str(), NULL, 10);

      // Generate and insert 'count' IDs and store the results.
      inserted = insertRands(theHash, count, IDlist, totalCollisions);

      // Report the number of records in the hash, the number
      // of records inserted, and the number of collisions
      cout << inserted << " records inserted." << endl;
      cout << totalCollisions << " collisions during inserts." << endl;
      cout << "The hash now contains " << theHash.count();
      cout << " records." << endl;
      cout << "Alpha = " << theHash.alpha() << endl;
      break;

      //--------------- Delete random ID's ----------------------------
    case 'B':
    case 'b':

      // Prompt for the number of ID's to delete.
      cout << endl << "How many random ID's to delete? ==> ";
      cout.flush();
      getline(cin, response);
      count = strtol(response.c_str(), NULL, 10);

      // Delete 'count' hash values and store the results
      deleted = deleteRandCountIDs(theHash, count, IDlist);

      if (deleted > 0) {
	// Report results
	cout << deleted << " records deleted.\n";
	cout << "The hash now contains " << theHash.count();
	cout << " records.\n";
	cout << "Alpha = " << theHash.alpha() << endl;
      } else {
	cout << "No records were deleted." << endl;
      }
      break;

      //--------------- Search for random ID's ----------------------------
    case 'C':
    case 'c':

      // Prompt for number of searches to perform
      cout << endl << "How many searches to perform? ==> ";
      cout.flush();
      getline(cin, response);
      count = strtol(response.c_str(), NULL, 10);

      // Perform 'count' searches for hash values and store the results
      found = searchHash(theHash, count, IDlist, totalProbes);

      if (found > 0) {
	// Report results
	cout << count << " searches performed." << endl;
	cout << totalProbes << " probes during searches." << endl;
      }
      break;

      //--------------- Insert a specific ID ----------------------------
    case 'D':
    case 'd':

      // Prompt for ID to insert
      cout << "Enter ID to insert: ";
      cout.flush();
      getline(cin, response);
      ID = strtol(response.c_str(), NULL, 10);

      if (ID <= 0) {
	cout << "Unable to insert ID: " << ID << endl;
      }

      // If the ID is valid, insert it into the hash
      else {
	success = theHash.insert(ID, totalCollisions);

	// Report results
	if (success) {
	  cout << "ID inserted, there were " << totalCollisions;
	  cout << " collisions." << endl;
	  cout << "The hash now contains " << theHash.count();
	  cout << " records." << endl;
	  cout << "Alpha = " << theHash.alpha() << endl;
	}
	else {
	  cout << "Unable to insert ID " << ID;
	  cout << " (duplicate ID or out of memory)." << endl;
	}
      }
      break;

      //--------------- Delete a specific ID ----------------------------
    case 'E':
    case 'e':

      // Prompt for ID to delete
      cout << "Enter ID to delete: ";
      cout.flush();
      getline(cin, response);
      ID = strtol(response.c_str(), NULL, 10);

      if (ID <= 0) {
	cout << "Unable to delete ID: " << ID << endl;
      }

      // If valid, remove the ID
      else {
	success = theHash.remove(ID);

	// Report results
	if (success)
	  cout << "ID " << ID << " deleted." << endl;
	else
	  cout << "Unable to delete ID " << ID << endl;

	cout << "The hash now contains " << theHash.count();
	cout << " records." << endl;
	cout << "Alpha = " << theHash.alpha() << endl;
      }
      break;

      //--------------- Search for a specific ID ----------------------------
    case 'F':
    case 'f':

      // Prompt for ID to find
      cout << "Enter ID to search for: ";
      cout.flush();
      getline(cin, response);
      ID = strtol(response.c_str(), NULL, 10);

      if (ID <= 0) {
	cout << "Unable to search for ID: " << ID << endl;
      }

      // If valid, search for the ID
      else {
	success = theHash.find(ID, probes);

	// Report results
	if (success) {
	  cout << "ID found, there were " << probes;
	  cout << " probes." << endl;
	}
	else {
	  cout << "ID: " << ID;
	  cout << " not found." << endl;
	}
      }
      break;

      //--------------- Quit ----------------------------
    case 'Q':
    case 'q':
      break;

    default:
      cout << "Unknown input: " << response << endl;
    }

  }

  return(0);
}
