#ifndef _typedef_header_
#define _typedef_header_
#include <iostream>
#include <string>
/* data type for gene feature */
typedef char gene_feature_t; 

/* data type for class */
typedef enum {positive, negative} gene_class_t;

/* discretized data group */
typedef char gene_group_t; 
#define HASHSIZE 3
int hashfunc(int value);
std::string itoa(const int &integer);
#endif	/* end of #ifndef */
