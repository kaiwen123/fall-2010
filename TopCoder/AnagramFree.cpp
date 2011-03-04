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
 
class AnagramFree {
public:
int getMaximumSubset(vector <string> S) {
  int len = S.size(); 
  for(int i = 0; i < len; i++) {
    sort(S[i].begin(), S[i].end());
  }

  int maxanagram = len; 

  for(int i = 0; i < len - 1; i++) {
    for(int j = i+1; j < len; j++) {
      if(S[i] == S[j]) {
	maxanagram--; 
	break;
      }
    }
  }
  return maxanagram; 
}


};
 


// Powered by FileEdit
// Powered by TZTester 1.01 [25-Feb-2003]
// Powered by CodeProcessor
