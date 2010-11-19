#ifndef _typedef_header_
#define _typedef_header_
#include <iostream>
#include <string>
#include <stdlib.h>
#include <assert.h>
#include <vector>
#include <queue>
#include <map>
#include <fstream>
#include <algorithm>		/* to use sort(). */
#include <cstdio> 		/* for sprintf() - DataSet.cpp*/

using namespace std;

/* data type for gene feature */
typedef char gene_feature_t; 

/* data type for class */
typedef enum {positive, negative} gene_class_t;

/* discretized data group */
typedef char gene_group_t; 
#define HASHSIZE 5
int hashfunc(int value);
std::string itoa(const int &integer);

/* Global variables. */
extern float minSupport, minConf;
extern int g, k, numTrans;

#endif	/* end of #ifndef */
