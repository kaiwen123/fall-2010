#include<stdio.h>
#include<math.h>

int main() {
  int test = 999; 
  double a=0.0, b=0.0, c=0.0; 
  printf("Please enter length of three size of the triangle:\n");
  //  printf("a = "); 
  scanf("%f %f %f", &a, &b, &c); 
  /* printf("b = ");  */
  /* scanf("%f", &b);  */
  /* printf("c = ");  */
  /* scanf("%f", &c);  */

  // debugging. 
  printf("a = %d\n", test); 

  double s, area; 
  s = (a + b + c) / 2; 
  /* error checking */
  // in case of errors, you should return an invalid value. 
  if(a <= 0) { printf("a should be bigger than 0."); return -1; }
  if(b <= 0) { printf("b should be bigger than 0."); return -1; }  
  if(c <= 0) { printf("c should be bigger than 0."); return -1; }

  if(a > s) { printf("a should be less than s\n"); return -1; }
  if(b > s) { printf("b should be less than s\n"); return -1; }
  if(c > s) { printf("c should be less than s\n"); return -1; }

  printf("%f, %f, %f, %f\n", a, b, c, s); 
  area = sqrt(s*(s-a)*(s-b)*(s-c)); 

  printf("The area of the triangle is : %f\n", area); 
  return area; 
}
