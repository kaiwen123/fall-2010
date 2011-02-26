/**
 *  Program to split a integer into the addition of several
 *  consecutive numbers. 
 *  Copyright (C) 2010 Simon Guo.
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see
 *  <http://www.gnu.org/licenses/>.
 */

#include <iostream>
#include <stdlib.h>
using namespace std; 

void testcons(int x); 

int main() {
  int MAX = 100;
  for(int i = 1; i < MAX; i++){
    int tmp = (int)((double)rand()/RAND_MAX*1000);
    testcons(tmp); 
  }
  return 0;
}

bool ispow(int x) {
  if(x < 2) return false;
  int tmp = x; 
  while(1) {
    if(tmp % 2 != 0) return false; 
    tmp /= 2; 
    if(tmp < 2) return false; 
  }
  return true;
}

/**
 * By convention, odd numbers can be splited into addition of two
 * consecutive numbers. But for even numbers, we need to consider
 * situations according to rules.
 * Firstly, if number can be divided by odd number, and the divisor is
 * odd number. Secondly, if number can be divided by even number with
 * residue 0.5. Also, if number is negative or if the number is power
 * of 2, it can not be represented as addition of consecutive
 * numbers. 
 */
void testcons(int x) {
  if(x <= 0) return;
  /* odd number. */
  if((x % 2) == 1) {
    cout << x << " = " << x / 2 
	 << " + " << x / 2 + 1 << endl;
    return; 
  } else {// even number. 
    if(ispow(x)) {
      cout << x << " FAIL!" << endl;
      return; 
    }
    int count = 2; 
    while(count <= x/2) {
      if(x % count == 0 && count % 2 == 1) {//count is odd
	int val = x/count - count / 2; 
 	if(val <= 0) return; 
	cout << x << " = ";
	for(int i = -1 * count / 2; i <= count / 2; i++) {
	  val = x/count + i; cout << val; 
	  if(i <= count / 2 - 1) cout << " + "; 
	} /* for */
	cout << endl; 
      }	/* if */
      if(float(x)/count - x/count == 0.5 && count % 2 == 0) {
	int val = x/count - count / 2 + 1; 
	if(val <= 0) return; 
	cout << x << " = ";
	for(int i = -1 * count / 2 + 1; i <= count / 2; i++) {
	  val = x/count + i; cout << val; 
	  if(i <= count / 2 - 1) cout << " + "; 
	} /* for */
	cout << endl; 
	return ;
      }
      count++; 
    }
  }
}
