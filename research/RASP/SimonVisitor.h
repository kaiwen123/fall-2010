/*Visitor class*/

#ifndef _spatialindex_simonvisitor_h_
#define _spatialindex_simonvisitor_h_

#include <SpatialIndex.h>
#include <fstream>
#include <cmath>
#include <string>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_blas.h>

using namespace std; 
namespace SpatialIndex 
{
  class SimonVisitor : public IVisitor {
  private:
    int numNodeVisited;
    int totalResult;		// number of result for step two; 
    int preresult;		// number of result of step one; 
    int currentQuery; 		// key for the original query file; 
    //    float filteringtime;	// time of the filtering process; 
    string qfile;		// Orignal query file for validation;
    bool needValidate;
    double totalFilterTime;
    double totalValidateTime;
    map<int, vector<float> > qlist;
  public:
    SimonVisitor();
    virtual ~SimonVisitor() {}
    virtual void visitData(std::vector<const IData*>& v) {}
    virtual void visitNode(const INode& n) {numNodeVisited++;}
    virtual void visitData(const IData& d);
    int getNumNodeVisited() { return numNodeVisited;}
    int getNumTotalResult() { return totalResult;}
    int getNumPreresult() {return preresult;}
    double getValidateTime() {return totalValidateTime;}
    double getFilterTime() {return totalFilterTime;}
    void incrementPreresult() {	preresult++;}
    void incrementFinalResult() {totalResult++;}
    void setCurrentQuery(int q) {currentQuery = q;}
    void setValidateQfile(string& q);
    bool validate(const string& s);
  };
}
#endif	// define. 
