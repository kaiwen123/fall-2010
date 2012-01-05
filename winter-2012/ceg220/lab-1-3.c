#include<stdio.h>
#include<math.h>

int main() {
  int test = 999; 
  double a=0.0, b=0.0, c=0.0; 
  printf("Please enter length of three size of the triangle:\n");
  //  printf("a = "); 
  scanf("%lf %lf %lf", &a, &b, &c); 
  /* printf("b = ");  */
  /* scanf("%f", &b);  */
  /* printf("c = ");  */
  /* scanf("%f", &c);  */

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
  
  area = sqrt(s*(s-a)*(s-b)*(s-c)); 

  printf("The area of the triangle is : %lf\n", area); 
  system("pause");
  return area; 
}
// tricks. 
// the format string for scanf should match the variable definition, or 
// else unpredicated consequence might happen. 
/*test cases;
3 4 5  --> 6.0000 
10 10 10 --> 43.3012 
-1 2 3 --> a should be bigger than 0. 
10 20 1000 --> c should be smaller than s. 
*/ 
