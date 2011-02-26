#include <fstream>
#include <string>
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;
// Declaration of functions and operations.
int findMax(vector<int> &a); 

int main (int argc, char *argv[]) {
  ifstream inputFile; 
  ofstream outputFile;
  int input; 
  char *inputFilename = "test.txt"; 
  char *outputFilename = (char *)"out.txt"; 
  cout << inputFilename << " - - " << argc << endl; 

  vector<int> value; 
  vector<int>::iterator value_it;

  if(argc == 3) {
    inputFilename = argv[1];
    outputFilename = argv[2];
  } else if(argc == 2) {
    inputFilename = argv[1];
  }
  //  cout << inputFilename << endl; 
  inputFile.open(inputFilename, ios::in); 
  outputFile.open(outputFilename, ios::out);
  if(inputFile) {
    while (!inputFile.eof()) {
      inputFile >> input; 
      cout << input << endl; 
      value.push_back(input);
    }
  } else {
    cerr << "Open file error, please check your files; " << endl; 
  }
  sort(value.begin(), value.end());

  // random_shuffle(value.begin(), value.end());
  //  copy(value.begin(), value.end(), ostream_iterator<int>(cout, " ")); 
  cout << "The maximum value of the array is : " << findMax(value) << endl; 

  for(value_it = value.begin(); value_it != value.end(); value_it++) {
    outputFile << *value_it << endl; 
  }
  return 0; 
}

/*
 * find the maximum nubmer of an array. 
 * @param vector<int> &a the vector that is used to manipulate. 
 * @output int The maximum value will be returned. 
 */
int findMax(vector<int> &a) {
  int max; 
  vector<int>::iterator it; 
  it = a.begin(); 
  for(max = *it; it != a.end(); it++) {
    if(max < *it) {
      max = *it;
      cout << "one time change....." << endl; 
    }
  }
  return max;
}
