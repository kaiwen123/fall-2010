#include <vector>
#include <list>
#include <map>
#include <set>
#include <queue>
#include <deque>
#include <stack>
#include <bitset>
#include <algorithm>
#include <functional>
#include <numeric>
#include <utility>
#include <sstream>
#include <iostream>
#include <iomanip>
#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <ctime>

using namespace std;


class QuadraticEquations {
public:
	long long howMany(int x, int y, int d, int z, int k) {
		long long num = 0; // actually 000 will always hold.  
		float s = (x + y * sqrt(d)) / z; 
		float e = 0.0001; 
		for(int i = -k; i <= k; i++) {
			for(int j = -k; j <= k; j++) {
				for(int m = -k; m <= k; m++) {
					if(abs(i * s * s + j * s + m) < e) num++;
				}
			}
		}
		return num; 
	}
};


<%:testing-code%>
//Powered by KawigiEdit 2.1.8 (beta) modified by pivanof!