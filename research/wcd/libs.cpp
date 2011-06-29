#include "libs.h"

string itoa(int integer){
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

// Usage of the program. 
void usage() {
  cerr << "USAGE:\n./wcd <trans_file> <fanout> <maxentry> <maxlevel>\n";
  cerr << "trans_file: file name of transaction.\n"
       << "fanout    : max child nodes for nonleaf (internal) node.\n"
       << "maxlevel  : maximum level of tree." << endl;
  return; 
}

// membership operation. 
void addMembership(int eid) {
  members.push_back(eid); 
  return; 
}

int getMembership(int index) {
  if(members.size() <= index) {
    cerr << "index error." << endl;
    return -1; 
  }
  return members[index]; 
}

bool changeMembership(int index, int neweid) {
  int oldeid = getMembership(index); 
  if(-1 == oldeid) {
    return false; 
  }
  cout << "Changing membership for trans "
       << index << " from " << oldeid
       << " to new eid " << neweid << endl; 
  members[index] = neweid; 
  return true; 
}

int getDegree() {return degree;}
void setDegree(int dg) {degree = dg; }

int getMaxLevel() {return maxlevel; }
void setMaxLevel(int lvl) {maxlevel = lvl; }

int getUplimit() {return uplimit; }

// @brief Setup total number of nodes in the tree according to
// fanout and level.
// @param none. 
// @return none.
void setUplimit() {
  // calculate total number of nodes tree can host. 
  // this should be calculated from level of tree and 
  // fanout of index nodes and max entry in leaf nodes. 
  int deg = getDegree(); 
  int maxlvl = getMaxLevel(); 
  num_nodes = 0; 
  uplimit = (int)pow(deg, maxlvl) - 1;
}

void incNumNodes() {num_nodes++;}
int getNumNodes() {return num_nodes;}
