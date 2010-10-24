#include <stdio.h>
#include <gsl/gsl_blas.h>
#include <iostream>
#include <string>
#include <map>
#include <SpatialIndex.h>
#include <queue>		// queue to store data. 
#include <stdlib.h>
#include "DataServer.h"

using namespace std;

int validate0(float*y, float* invA, float* W, int k);
float validate(float* y, float* T, int k);
string insertData(string data); 
string queryData(string query);
