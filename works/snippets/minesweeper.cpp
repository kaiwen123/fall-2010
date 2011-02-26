#include <iostream>
#include <vector>
#include <string>
#include <fstream> 

using namespace std;

void usage();

int main(int argc, char* argv[]) {
  int row, col, num; 
  ifstream inputFile; 
  string r; 
  char x;

  if(argc != 2) {
    usage();
    return 1; 
  }
  inputFile.open(argv[1],ios::in);
  if(inputFile == NULL){
    cerr << "Open file error, please check your files. " << endl; 
    return 1;
  }
  inputFile >> row >> col; 
  cout << "row = " << row << endl 
       << "col = " << col << endl; 
  //  matrix<char> grid(row, col);
  char grid[row][col];

  for(int i = 0; i < row; i++) {
    inputFile >> r; 
    if(r.size() > col) {
      cerr << "Grid is wrong, please check....." << endl; 
      return 1;
    }
    // put the grid chars into matrix; 
    for(int j = 0; j < col; j++) {
      grid[i][j] = r[j];
      // cout << grid[i][j] << endl
      //   << (i + 1) * (j + 1) << endl; 
    }
  }
  //cout << row << ", " << col << endl; 
  for(int i = 0; i < row; i++) {
    for(int j = 0; j < col; j++) {
      num = 0;			// the number of mine; 
      //cout << grid[i][j]; 
      if(grid[i][j] == '?') {
       	//cout << endl << i << ", " << j << endl; 
	// left; 
	if(j - 1 >= 0) {
	  if(grid[i][j - 1] == 'o') {
	    num += 1;
	  }
	}
	// right;
	if(j + 1 < col) {
	  if(grid[i][j + 1] == 'o') {
	    num += 1;
	  }
	}

	// up; 
	if(i - 1 >= 0) {
	  if(grid[i - 1][j] == 'o') {
	    num += 1;
	  }
	}

	// down;
	if(i + 1 < row) {
	  if(grid[i + 1][j] == 'o') {
	    num += 1;
	  }
	}
	cout << "number is : = " << num << endl; 
  	grid[i][j] = num + '0'; 
      }
    }
    //cout << endl;
  }
  // Output the grid now ... 
  for(int i = 0; i < row; i++) {
    for(int j = 0; j < col; j++) {
      cout << grid[i][j];
    }
    cout << endl; 
  }
  return 0;
}
void usage () {
  cout << "Usage: minesweeper [FILENAME]" << endl; 
}
