/* c_ext.i */
%module c_ext
%include "cpointer.i"
/* Map array to a new type in python */
%include "carrays.i"
%array_class(float, floatArray);
%{  
   /* Put header files here or function declarations like below */
  extern int validate0(float*y, float* invA, float* W, int k);
  extern float validate(float* y, float* T, int k);
   %}

extern int validate0(float*y, float* invA, float* W, int k);
extern float validate(float* y, float* T, int k);
