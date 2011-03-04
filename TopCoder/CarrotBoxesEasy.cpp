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
 
class CarrotBoxesEasy {
public:
int theIndex(vector <int> carrots, int K) {
  int index = 0;		// the index to return. 
  int size = carrots.size(); 	// number of boxes. 
  int largest; 			// largest box index; 
  for(int i = 0; i < K; i++) {
    // find box index with largest number of carrots. 
    largest = 0; 
    for(int j = 0; j < size  ; j++) {
      if(carrots[largest] < carrots[j]) {	
	largest = j; 
      }
    }
    carrots[largest]--; 
    index = largest; 
  }
  return index; 
}


};
 


// Powered by FileEdit
// Powered by TZTester 1.01 [25-Feb-2003]
// Powered by CodeProcessor
