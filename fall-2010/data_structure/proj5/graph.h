// -*- C++ -*-
/**
 * @file Graph.h
 * @brief Definition of a Graph class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.1
 */
// $Log$

#ifndef _GraphClass_
#define _GraphClass_
#include <string>
#include <vector>
#include <fstream>
#include <iostream>
#include "node.h"

using namespace std; 

class Graph {
 private:
  vector<vnode> vertex;	// Vertex vectors. 

 public:
  Graph(){}
  ~Graph(){vertex.clear();}
  // getters. 
  int getNumVertices() const {return vertex.size();}

  // Load graph structure from file to adjacency list. 
  bool loadFileToAdjList(string fname); 

  // Depth first search. 
  bool DFSTraversal(int id);

  // Breadth first search. 
  bool BFSTraversal(int id);

  // Dijkstra's shortest path algorithm to find a path from vertex id to
  // all other vertices.
  bool DijSP(int id);

  // Print the adjacency list. 
  bool printAdjList(); 
};
#endif	/* ifdef */
