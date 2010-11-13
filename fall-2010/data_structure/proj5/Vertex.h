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

class Vertex {
 private:
  int id; 			/* vertex id. */
  int weight; 			/* Weight to this vertex. */
  bool visited;			/* if this vertex is visited. */
  Vertex *nxt;			/* pointing to next vertex. */

 public:
 Vertex(int i):id(i),weight(0),visited(false),nxt(NULL) {
    cout << "Creating new vertex...." << id << endl;
} 
  ~Vertex(){}
  // getters. 
  int getId() const {return id;}
  int getWeight() const {return weight;}
  Vertex* next() {return nxt;} 
  bool isVisited() const {return visited;}

  // setters. 
  void setId(int i) {id = i;}
  void setVisited() {visited = true;}
  void setNext(Vertex* n) {nxt = n;}
  void setWeight(int wt) {weight = wt;}
  friend ostream& operator<<(ostream& out, Vertex& v) {
    out << v.getId(); 
    return out; 
  }
  void visit() {
    cout << getId() << ":" << getWeight() << endl; 
    setVisisted();
  }
};
#endif	/* ifdef */
