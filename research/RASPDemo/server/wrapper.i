/* c_ext.i */
%module rasp_demo
%include "cpointer.i"

 /* Map array to a new type in python */
%include "carrays.i"
%include "std_string.i"

%array_class(float, floatArray);
%{
#define SWIG_FILE_WITH_INIT
  %}
%{  
  /* Put header files here or function declarations like below */
  //typedef std::string String;
  using namespace std;
  extern string insertData(string data); 
  extern string queryData(string query);
  %}
extern std::string insertData(std::string data);
extern std::string queryData(std::string query);
