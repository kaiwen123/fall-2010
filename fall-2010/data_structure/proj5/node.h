// -*- C++ -*-
/**
 * @file Vertex.h
 * @brief Definition of a Vertex class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.1
 */
// $Log$

#ifndef _VertexClass_
#define _VertexClass_
#include <string>
#include <vector>
#include <iostream>

using namespace std;
// Vertex node.
class enode;  
class vnode {
private: 
  int id; // Id of the vertex node. 
  bool visited; // if node has been visited. 
  enode *edge; // Edge connecting this node. 
public:
  vnode(int i):id(i),visited(false),edge(NULL){}
  bool isVisited() const {return visited;}
  int getId() const {return id;}
  enode *firstEdge() {return edge;}

  void visit() {visited = true;}
  void reset() {visited = false;}
  void setFirstEdge(enode *en) {edge = en;}

  void printAdj() {
    enode *pnode = edge;
    while(pnode) {
      cout << *pnode << " "; 
    }
    cout << endl; 
  }
};

// Edge node. 
class enode {
private: 
  int id; 			// id of vertex node. 
  int weight;			// Weight of edge.
  enode *next;			// next edge connecting the node. 
public: 
  enode(int i, int w):id(i),weight(w),next(NULL){}
  int getId() const {return id;}
  int getWeight() const {return weight;}
  enode* next() {return next;}
  void setNext(enode *ne) {next = ne;}
  friend ostream& operator<<(ostream& out, enode& e) {
    out << e.getId(); 
    return out; 
  }
}; 

#endif	/* ifdef */
