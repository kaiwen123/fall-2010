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


class NiceOrUgly {
public:
	string describe(string s) {
		int wov = 0, con = 0; 
		int len = s.length(); 
		bool q = false; 
		for(int i = 0; i < len; i++) {
			if (s[i] == '?') {
				q = true; 
				if (wov >= 3 || con >= 5) return "UGLY"; 
				wov++; con++; continue; 
			}
			if (s[i] == 'A' || s[i] == 'E' || s[i] == 'I' || s[i] == 'O' || s[i] == 'U') {
				wov++; 
				if (con < 5) con = 0; 
			} else {		
				con++; 
				if (wov < 3) wov = 0; 
			}
		}
		if(q) {
			if(wov >= 3 && con >= 5) return "UGLY"; 
			if(wov < 3 && con < 5) return "NICE";
			return "42"; 
		} else {
			if(wov >= 3 || con >= 5) return "UGLY"; 
			return "NICE"; 
		}
	}
};


<%:testing-code%>
//Powered by KawigiEdit 2.1.8 (beta) modified by pivanof!