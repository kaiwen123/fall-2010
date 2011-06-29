/* 
 * @file libs.h This file defines utilities used for the program. 
 * @author Simon Guo <gsmsteve@gmail.com> 
 */
#ifndef _LIBS_H_
#define _LIBS_H_
#include <string>
#include <iostream>
#include <vector>
#include <math.h>

using namespace std;

/* usage prompt for the program. */
void usage(); 

/* function to change integer to string. */
string itoa(int integer);

/* entry test operations. */
enum t_type {
  ADD, 
  REMOVE 
};

/* transaction membership. */
static vector<int> members;	/* membership for each trans. */
void addMembership(int eid);
int getMembership(int index);
bool changeMembership(int index, int neweid);

/* tree node configuration variables. */
static int degree; 		/* degree of node. */
static int maxlevel; 		/* maximum level of tree. */
static int uplimit; 		/* uplimit number of nodes. */
static int num_nodes;		/* total node count. */

// degree
int getDegree();
void setDegree(int dg);

// level.
int getMaxLevel(); 
void setMaxLevel(int lvl); 

// total node count. 
int getUplimit();
void setUplimit();

void incNumNodes(); 
int getNumNodes(); 

#define SPLIT 1 
#define NONE 0 

#endif
