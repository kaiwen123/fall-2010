/* c_ext.i */
%module rasp_demo
%include "cpointer.i"

/* Map array to a new type in python */
%include "carrays.i"
%include "std_string.i"

%array_class(float, floatArray);
%{
#define SWIG_FILE_WITH_INIT
#include "wrapper.h"
#include <string>
%}
%{  
   /* Put header files here or function declarations like below */
  //typedef std::string String;
  using namespace std;
  extern int validate0(float*y, float* invA, float* W, int k);
  extern float validate(float* y, float* T, int k);
  extern string insertData(string data); 
  extern string queryData(string query);
  %}
extern std::string insertData(std::string data);
extern std::string queryData(std::string query);
extern int validate0(float*y, float* invA, float* W, int k);
extern float validate(float* y, float* T, int k);
