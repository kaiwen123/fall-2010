#include <functional>
#include <algorithm>
#include <stdexcept>
#include <iostream>
#include <sstream>
#include <fstream>
#include <numeric>
#include <iomanip>
#include <cstdlib>
#include <cstring>
#include <utility>
#include <cctype>
#include <vector>
#include <string>
#include <bitset>
#include <cmath>
#include <queue>
#include <stack>
#include <ctime>
#include <list>
#include <map>
#include <set>
 
using namespace std;
 
#define PB push_back
#define MP make_pair
 
typedef vector<int> VI;
typedef vector<string> VS;
typedef vector<double> VD;
typedef pair<int,int> PII;
typedef long long LL;
typedef unsigned long long ULL;
 
class MakeSquare {
public:

  // This is a really hard problem for me currently, 
  // I hope someday i can program such a problem very easily. 

int minChanges(string S) {
  int len = S.size(); ls
  int numchanges = 0; 

  // First, find the best split for the string. The goal is to get the
  // minimum difference for the two halves. 
  int first = 0, second = 1; 
  int bestsplit = 0, diff = 10000; 
  int len1, len2;
  string str1, str2; 
  if(len == 1) return 1; 	// simply add the same char to the end. 
  for(second = 1; second < len; second++) {
    str1 = S.substr(first, second - first); 
    str2 = S.substr(second, len - second); 
    int splitdiff = getDiff(str1, str2); // difference for current split.
    // test if current split is a better one. 
    if(diff > splitdiff) {
      diff = splitdiff; 
      bestsplit = second;
    }
    if(0 == diff) break; 
  }

  if(0 == diff) return 0;	 // two halves are the same.
  str1 = S.substr(first, bestsplit); 
  str2 = S.substr(bestsplit, len - str1.size()); 
  // cout << "Init: " << str1 << " "  << str2 << endl; 

  // Second, compare char by char for the two halves and try to add,
  // delete and change chars to make a decision on each different
  // char.
  int diffdeladd, diffchange; 
  len1 = str1.size(); len2 = str2.size();
  for(int i = 0; i < (len1 >= len2 ? len1 : len2); i++) {
    if(0 == getDiff(str1, str2)) break; // equal.
    if((0 == str1.size()) || (0 == str2.size())) {
      numchanges += str1.size() + str2.size(); 
      break;
    } 

    if(str1[0] == str2[0]) {
      str1 = str1.substr(1, str1.size()); 
      str2 = str2.substr(1, str2.size());
    } else {
      // test add/del;
      // add one char in str1 is equivalent to delete one char from str2. 
      int diffdeladd1 = getDiff(str1, str2.substr(1, str2.size()-1)); 
      int diffdeladd2 = getDiff(str1.substr(1, str1.size()-1), str2);
      diffdeladd = diffdeladd1 < diffdeladd2 ? diffdeladd1 : diffdeladd2; 

      // test change; 
      diffchange = getDiff(str1.substr(1,str1.size()-1), str2.substr(1,str2.size()-1)); 

      if(diffdeladd < diffchange) {
	if(diffdeladd1 <= diffdeladd2) {
	  str2 = str2.substr(1, str2.size()-1);
	} else {
	  str1 = str1.substr(1, str1.size()-1);
	}
      } else {
	str1 = str1.substr(1, str1.size() - 1);
	str2 = str2.substr(1, str2.size() - 1);
      }
      numchanges++;
    }
    cout << str1 << " " << str2 << " " <<  numchanges << endl; 
  }
  return numchanges; 
}

  // get the number of different chars of two strings. 
  int getDiff(string str1, string str2) {
    int retval = 0; 
    int len1 = str1.size(), len2 = str2.size(); 
    int lendiff = abs(len1 - len2); 
    for(int i = 0; i < (len1 < len2 ? len1 : len2); i++) {
      if(str1[i] != str2[i]) retval++; 
    }
    retval += lendiff; 
    return retval; 
  }

// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); if ((Case == -1) || (Case == 4)) test_case_4(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const int &Expected, const int &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { string Arg0 = "abcdabgcd"; int Arg1 = 1; verify_case(0, Arg1, minChanges(Arg0)); }
	void test_case_1() { string Arg0 = "abcdeabce"; int Arg1 = 1; verify_case(1, Arg1, minChanges(Arg0)); }
	void test_case_2() { string Arg0 = "abcdeabxde"; int Arg1 = 1; verify_case(2, Arg1, minChanges(Arg0)); }
	void test_case_3() { string Arg0 = "aabcaabc"; int Arg1 = 0; verify_case(3, Arg1, minChanges(Arg0)); }
	void test_case_4() { string Arg0 = "qetuoadgjlzcbmqwertyuiopasdfghjklzxcvbnm"; int Arg1 = 2; verify_case(4, Arg1, minChanges(Arg0)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    MakeSquare ___test;
    ___test.run_test(-1);
}
// END CUT HERE
