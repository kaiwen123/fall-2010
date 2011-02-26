#include <iostream> 
#include <stdlib.h> 
#include <vector> 

using namespace std; 

// Implementation of the dutch flag algorithm. 
// Detailed information about this algorithm can be found in the
// following link. 
// http://www.csse.monash.edu.au/~lloyd/tildeAlgDS/Sort/Flag/
// 
// ALGORITHM DESCRIPTION
// Two-partition algorithm. 
// space is partitioned to three subsections; 
// 0 : 0 --- Lindex - 1; 
// u : Lindex --- Hindex; The unknown index. 
// 1 : Hindex + 1 --- N; 			  
//
//|------------|-----------------|----------------|
//|0000...00000|xxxx.........xxxx|11111......11111|
//|------------|-----------------|----------------|
// ^   	       	^      	       	^      	       	 ^
// |	  0    	|    Unknown   	|      	 1	 |
// 0   	       Low     	       High	       	 N
//
//|-------|---------|-------------------|---------|
//|00...00|111...111|xxxxxxx.......xxxxx|222...222|
//|-------|---------|-------------------|---------|
// ^   	   ^   	     ^ 	       	       ^       	 ^
// |   0   |   	1    | 	   Unknown     |    2	 |
// 0      Low  	    Mid	      	      High   	 N
// 					     	 
	  				     
// Three-partition algorithm. 		       	 
int main() {   				     
  // two partion implementation. 	     
  cout << "Two-partition problem:" << endl; 
  int N = 100; 			// total number of 
  int Low = 0, Mid = 0, High = N; // low and high indexes; 
  vector<int> flags; 		// flag array. 
  for(int i = 0; i < N; i++) {		     
    float rnum = (float)rand() / RAND_MAX;   
    flags.push_back(rnum <= 0.5 ? 0 : 1);    
  }	       
  // Shrink the unknown area and add everything to either area. 
  int tmp;     
  while(Low != High) {
    if(flags[Low] == 0) {
      Low++;   
    } else {   
      tmp = flags[Low]; 
      flags[Low] = flags[High]; 
      flags[High] = tmp; 
      High--; 
    }
  }
  // print the result. 
  vector<int>::iterator iter2 = flags.begin(); 
  while(iter2 != flags.end()) {
    cout << *iter2 << " "; 
    iter2++; 
  }
  cout << endl; 

  // three-partition problem. 
  // 
  // generate random index; 
  cout << endl << "Three-partition problem:" << endl; 
  flags.clear(); 
  for(int i = 0; i < N; i++) {
    float rnum = (float)rand() / RAND_MAX; 
    if(rnum < 0.33) flags.push_back(0);
    if(rnum >= 0.33 && rnum < 0.66) flags.push_back(1); 
    if(rnum >= 0.66) flags.push_back(2);
  }

  Low = Mid = 0; High = N; 
  
  while(Mid != High) {
    if(0 == flags[Mid]) {
      tmp = flags[Low]; 
      flags[Low] = flags[Mid]; 
      flags[Mid] = tmp; 
      Low++; Mid++; 
    } else if(1 == flags[Mid]) {
      Mid++; 
    } else {
      tmp = flags[Mid]; 
      flags[Mid] = flags[High];
      flags[High] = tmp; 
      High--; 
    }
  }
  // print the result. 
  vector<int>::iterator iter3 = flags.begin(); 
  while(iter3 != flags.end()) {
    cout << *iter3 << " "; 
    iter3++; 
  }
  cout << endl; 
  return 0; 
}
