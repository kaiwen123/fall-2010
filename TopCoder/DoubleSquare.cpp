/**
 * @brief A double-square number is an integer X which can be
 * expressed as the sum of two perfect squares. For example, 10 is a
 * double-square because 10 = 32 + 12. Your task in this problem is,
 * given X, determine the number of ways in which it can be written as
 * the sum of two squares. For example, 10 can only be written as 32 +
 * 12 (we don’t count 12 + 32 as being different). On the other hand,
 * 25 can be written as 52 + 02 or as 4^2 + 3^2.
 * 
 * @Input
 * You should first read an integer N, the number of test cases. The
 * next N lines will contain N values of X.
 * Constraints
 * 0 ≤ X ≤ 2147483647
 * 1 ≤ N ≤ 100 
 *
 * @Output
 * For each value of X, you should output the number of ways to write X
 * as the sum of two squares. 
 */

#include <iostream>
#include <stdlib.h>
#include <math.h>

using namespace std; 

int doubleSquare(unsigned int m); 
int main() {
  cout << doubleSquare(1215306625) << endl; 
  return 0; 
}

int doubleSquare(unsigned int m) {
  int p = sqrt((double)m / 2.0); //cout << "p = " << p << endl; 
  int total = 0; 
  for(int i = 0; i <= p; i++) {
    double j = sqrt((double)m - i*i); 
    //cout << i << " " << j << endl; 
    if(j - (int)j == 0.0) total++; 
  }
  return total; 
}
