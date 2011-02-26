
#include <list>
#include <iterator> 
#include "visitor.h"

void Foo::accept(Visitor* v) {v->visit(this); }
void Bar::accept(Visitor* v) { v->visit(this); }
void Baz::accept(Visitor* v) { v->visit(this); }

int main()
{
  list<Element*> e;
  list<Visitor*> v;
  typedef list<Element*>::const_iterator e_list_iterator;
  typedef list<Visitor*>::const_iterator v_list_iterator;
 
  e.push_back(new Foo);
  e.push_back(new Foo);
  e.push_back(new Bar);
  e.push_back(new Baz);
 
  CountVisitor* cv = new CountVisitor;
  v.push_back(cv);
  v.push_back(new MakeVisitor);
 
  for (e_list_iterator i = e.begin(); i != e.end(); ++i)
    for (v_list_iterator j = v.begin(); j != v.end(); ++j)
      (*i)->accept(*j);  // double dispatch
 
  cv->print_counters();
 
  for (e_list_iterator i = e.begin(); i != e.end(); ++i) delete *i;
  for (v_list_iterator j = v.begin(); j != v.end(); ++j) delete *j;
 
  return 0;
}
