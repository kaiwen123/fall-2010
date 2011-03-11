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
 
class TimeTravellingGardener {
public:
int determineUsage(vector <int> distance, vector <int> height) {
  vector<int> mincut; 
  float k = 0.0;		// slope of the line; 
  int len = distance.size(), numtrees = len + 1; 
  // set up the distance for each tree to the origin in x axis. 
  int dis = 0; 
  vector<int> xaxis; 
  vector<int> yaxis = height; 
  xaxis.push_back(dis); 
  for(int i = 0; i < len; i++) {
    dis += distance[i]; 
    xaxis.push_back(dis); 
  }

  // find the shortest tree as one point of the line. 
  int minhtree = height[0]; // mean height tree;
  vector<int> p1s;
  p1s.push_back(0);
  for(int i = 1; i < numtrees; i++) {
    if(height[i] < minhtree) {
      minhtree = height[i]; 
      p1s.clear(); 
      p1s.push_back(i);  
    } else if (height[i] == minhtree) {
      p1s.push_back(i); 
    }
  }

  // Then, let's try to construct a line for each tree. 
  int nump1 = p1s.size(); 
  for(int m = 0; m < nump1; m++) {
    int p1 = p1s[m]; 	// first point of the line. 
    float intersect = 0.0; 
    for(int i = 0; i < numtrees; i++) {
      if(i != p1) {
	int p2 = i; 
	k = (float)(yaxis[p2] - yaxis[p1]) / (float)(xaxis[p2] - xaxis[p1]);
	intersect = (float)yaxis[p1] - (float)xaxis[p1] * k; 
	// tree to test if other trees should be cut. 
	// if yes, continue; if no, stop and try another p2.
	bool ok = true; 
	int cutnum = 0;
	for(int j = 0; j < numtrees; j++) {
	  if((j != p1) && (j != p2)) {
	    float y = k * xaxis[j] + intersect; 
	    if((y > (float)yaxis[j]) || (y < -0.01)) {
	      ok = false; break; 
	    } else if (y < (float)yaxis[j]) {
	      cutnum++; 
	    }
	  } // if
	}   // for test all other trees. 
	if(ok) mincut.push_back(cutnum); 
      }
    } // for point two. 
  } // for point one. 
  sort(mincut.begin(), mincut.end()); 
  return mincut.size() == 0 ? numtrees - 1 : mincut[0]; 
}

// BEGIN CUT HERE
	public:
	void run_test(int Case) { if ((Case == -1) || (Case == 0)) test_case_0(); if ((Case == -1) || (Case == 1)) test_case_1(); if ((Case == -1) || (Case == 2)) test_case_2(); if ((Case == -1) || (Case == 3)) test_case_3(); if ((Case == -1) || (Case == 4)) test_case_4(); if ((Case == -1) || (Case == 5)) test_case_5(); if ((Case == -1) || (Case == 6)) test_case_6(); }
	private:
	template <typename T> string print_array(const vector<T> &V) { ostringstream os; os << "{ "; for (typename vector<T>::const_iterator iter = V.begin(); iter != V.end(); ++iter) os << '\"' << *iter << "\","; os << " }"; return os.str(); }
	void verify_case(int Case, const int &Expected, const int &Received) { cerr << "Test Case #" << Case << "..."; if (Expected == Received) cerr << "PASSED" << endl; else { cerr << "FAILED" << endl; cerr << "\tExpected: \"" << Expected << '\"' << endl; cerr << "\tReceived: \"" << Received << '\"' << endl; } }
	void test_case_0() { int Arr0[] = {2,2}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {1,3,10}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 1; verify_case(0, Arg2, determineUsage(Arg0, Arg1)); }
	void test_case_1() { int Arr0[] = {3,3}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {3,1,3}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 2; verify_case(1, Arg2, determineUsage(Arg0, Arg1)); }
	void test_case_2() { int Arr0[] = {1,3}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {4,4,4}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 0; verify_case(2, Arg2, determineUsage(Arg0, Arg1)); }
	void test_case_3() { int Arr0[] = {4,2}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {9,8,5}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 1; verify_case(3, Arg2, determineUsage(Arg0, Arg1)); }
	void test_case_4() { int Arr0[] = {476,465,260,484}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {39,13,8,72,80}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 3; verify_case(4, Arg2, determineUsage(Arg0, Arg1)); }
	void test_case_5() { int Arr0[] = {173,36,668,79,26,544}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {488,743,203,446,444,91,453}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 5; verify_case(5, Arg2, determineUsage(Arg0, Arg1)); }
	void test_case_6() { int Arr0[] = {245, 245, 245, 490, 245, 245, 245, 245, 245, 245, 245, 245}; vector <int> Arg0(Arr0, Arr0 + (sizeof(Arr0) / sizeof(Arr0[0]))); int Arr1[] = {29, 62, 95, 128, 194, 227, 260, 293, 326, 359, 392, 425, 458}; vector <int> Arg1(Arr1, Arr1 + (sizeof(Arr1) / sizeof(Arr1[0]))); int Arg2 = 0; verify_case(6, Arg2, determineUsage(Arg0, Arg1)); }

// END CUT HERE

};
 
// BEGIN CUT HERE
int main() {
    TimeTravellingGardener ___test;
    ___test.run_test(-1);
}
// END CUT HERE
