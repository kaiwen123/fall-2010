/* c_ext.i */
%module rasp_demo
%include "cpointer.i"

/* Map array to a new type in python */
%include "carrays.i"
%array_class(float, floatArray);
%{
#define SWIG_FILE_WITH_INIT
#include "wrapper.h"
%}
%{  
   /* Put header files here or function declarations like below */
  using namespace std;
  int validate0(float*y, float* invA, float* W, int k);
  float validate(float* y, float* T, int k);
  string insertData(string data); 
  string queryData(string query);
  %}
/* extern string insertData(string data);  */
/* extern string queryData(string query); */
/* extern int validate0(float*y, float* invA, float* W, int k); */
/* extern float validate(float* y, float* T, int k); */
