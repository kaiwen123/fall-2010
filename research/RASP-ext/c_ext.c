/* File : time.c */
 
#include <stdio.h>
#include <gsl/gsl_blas.h>

float validate(float* y, float* T, int k) {
  /* 
     result = y^T * T * y
     Steps of evaluation: 
     1: y^T * T = x (1xk)
     2: x * y = result (1x1)
   */
  float result = 0.0, x[k];
  int i, j;
  for(i = 0; i < k; i++){
    x[i] = 0.0; 
  }
  for(i = 0; i < k; i++){
    for(j = 0; j < k; j++) {
      x[i] += y[j] * T[k*j + i]; 
    }
    result += x[i] * y[i]; 
  }
  return result; 
}

int validate0(float* y, float* invA, float* W, int k) {
  int i, j; 
  float x[k], x1[k];
  for(i = 0; i < k; i++){
    x[i] = x1[i] = 0.0;
  } 
  for(i = 0; i < k; i++) {
    for(j = 0; j < k; j++) {
      x[i] += invA[k*j + i] * y[2*j];
      x1[i] += invA[k*j + i] * y[2*j + 1];
      //printf("invA : %f, %f\n", invA[k*j + i], y[2*j]);
    }
    //printf("%f, %f\n", x[i], x1[i]);
  }
  /* y */
  float w[2*(k-2)], w1[2*(k-2)];
  for(i = 0; i < 2*(k-2); i++){
    w[i] = w1[i] = 0.0;
  } 
  for(i = 0; i < 2*(k-2); i++) {
    for(j = 0; j < k; j++) {
      w[i] += x[j] * W[2*(k-2)*j + i]; 
      w1[i] += x1[j] * W[2*(k-2)*j + i]; 
    }
    //printf("%f, %f, ", w[i], w1[i]);
  }
  float r = 0.0, r1 = 0.0; 
  for(i = 0; i < k; i++){
    r += invA[k*(i+1)-1] * y[2*i];
    r1 += invA[k*(i+1)-1] * y[2*i+1];
    //printf("%f, %f, ", y[2*i], y[2*i+1]);
  }
  //printf("%f, %f \n", r, r1);
  for(i = 0; i < 2*(k-2); i++) {
    //printf("%f, %f, ", w[i], w1[i]);
    //printf("%f, %f, \n", r*w[i], r1*w1[i]);
    if (!((r * w[i] <= 0) && (r1 * w1[i] <= 0)))
       return 0;
    else
      continue; 
  }
  return 1;
}
