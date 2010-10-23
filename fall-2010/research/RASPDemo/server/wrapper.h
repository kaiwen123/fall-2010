#include <stdio.h>
#include <gsl/gsl_blas.h>
#include <iostream>
#include <string>

#include "DataServer.cpp"

using namespace std;

int validate0(float*y, float* invA, float* W, int k);
float validate(float* y, float* T, int k);
string insertData(string data); 
string queryData(string query);
