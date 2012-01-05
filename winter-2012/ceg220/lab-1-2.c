
/*************************************************************************************
Author: your name         
Lab: Laboratory 1         
Class: CEG 220
      File Name: yname-lab-1.c (where yname is your name)         
Instructor: DeJongh                     
Due Date: [due date of current assignment]
    Overview:
This program calculates the volume of a sphere or the volume of a cube.
It prompts the user to enter an integer which identifies the case to be computed.
Program Structure:
The program prints heading information, followed by a menu structure which prompts
the user to enter a 1 or a 2 to chose the volume computation for a sphere or a 
cube, respectively.  An if-else structure is used to determine which formula to
    use for the computation, and which message is to be printed. If an invalid choice 
is entered, a message is printed and the program terminates.
    
Major Variables:
          selection    represents the user's input choice
radius       represents the radius of sphere (meters)
length       represents the length of a side of the cube (meters)
          volume       represents the volume of the object, (cubic meters)
    Results/Observations (when required)
E.G. Increasing the length causes the volume of the cube to increase.
etc.                    
**************************************************************************************/ 
#include <stdio.h> 
#include <math.h>
#define PI 3.141592 
int main(void) 
{ 
  int selection;   /* The User's selection identifying which volume to compute.  */ 
  double radius;    /* Radius of the sphere in meters                            */
  double length;    /* Length of a side of the cube in meters                     */ 
  double volume;   /* Volume in cubic meters.                                    */ 
  /* Introductory input
     Prompt the user to enter an integer:
     1 to choose a sphere, or 2 to choose a cube.          
  */
  printf("\n\nThis program will compute the volume of a sphere or\n"); 
  printf("the volume of a cube.\n"); 
  printf("\nSelect by number:\n"); 
  printf("1] Volume of a sphere. 2] Volume of a cube.\n"); 
  printf("Your selection (1 or 2) = "); 
  scanf("%d",&selection); 
  /* Compute volume based on selection.
     If selection = 1, prompt the user to enter the radius of the sphere, calculate the   
     volume, and print the result to the screen. If selection = 2, prompt the user to
     enter the length of a side of the cube, calculate the volume, and print the result 
     to the screen
  */
  if (selection == 1) 
    { 
      printf("Please enter the radius of the sphere (meters) = "); 
      scanf("%lf",&radius);           /* Watch out: The "l" in "lf" is lower case "L" */
      volume = (4.0/3.0)*PI*pow(radius,3); 
      printf("A sphere of radius %lf has a volume of %lf cubic meters",radius, volume); 
    } 
  else if (selection == 2) 
    { 
      printf("Please enter the length of the side of the cube (meters) = "); 
      scanf("%lf",&length); 
      volume = length * length * length; 
      printf("A cube with side %lf has a volume of %lf cubic meters",length, volume); 
    }
  /*  If user input was invalid, print message and exit */ 
  else 
    { 
      printf("That was not one of the selections.\n"); 
      printf("You must run the program again and\n"); 
      printf("select either a 1 or a 2.\n"); 
    }
  /* End the program */
  printf("\n\nThis concludes the program to calculate\n"); 
  printf("the volume of a sphere or of a cube.\n\n"); 
   
  system("pause"); 
  return(0); 
}  // end main
