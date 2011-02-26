#include <iostream>

using namespace std; 

// This is used to show the visitor pattern; 

class Visitor; 

class Element {
public:
  virtual ~Element(); 
  virtual void accept(Visitor*); 
}; 

class Foo : public Element {
public:
  void make_foo(){ cout << "Making Foo" << endl; } 
  virtual void accept(Visitor*); 
};

class Bar : public Element {
public:
  void make_bar(){ cout << "Making Bar" << endl; }
  virtual void accept(Visitor*); 
}; 

class Baz : public Element {
public:
  void make_baz(){ cout << "Making Baz" << endl; }
  virtual void accept(Visitor*); 
};

class Visitor {
public: 
  virtual ~Visitor();
  virtual void visit(Foo*) = 0;
  virtual void visit(Bar*) = 0; 
  virtual void visit(Baz*) = 0; 
};

class MakeVisitor : public Visitor {
  virtual void visit(Foo* foo) { foo->make_foo(); }
  virtual void visit(Bar* bar) { bar->make_bar(); }
  virtual void visit(Baz* baz) { baz->make_baz(); }
}; 

class CountVisitor : public Visitor {
public:
  CountVisitor() : foo_count(0), bar_count(0), baz_count(0) { }
  void print_counters() { cout << "Counters: Foo(" 
			       << foo_count << ") Bar("
			       << bar_count << ") Baz(" 
			       << baz_count << ")" << endl; }
  
  virtual void visit(Foo*) { ++foo_count; }
  virtual void visit(Bar*) { ++bar_count; }
  virtual void visit(Baz*) { ++baz_count; }
private:
  int foo_count, bar_count, baz_count;
}; 
