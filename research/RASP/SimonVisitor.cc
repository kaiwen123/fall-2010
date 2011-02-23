#include "SimonVisitor.h"
using namespace SpatialIndex;
using namespace std; 

SpatialIndex::SimonVisitor::SimonVisitor() {
  numNodeVisited = 0;
  totalResult = 0;
  needValidate = false;
  preresult = 0; 
  totalFilterTime = 0.0;
  totalValidateTime = 0.0;
}

void SpatialIndex::SimonVisitor::setValidateQfile(string& q){
  qfile = q; 
  needValidate=true;
  // Now building the hashtable; 
  cout << "Building hash table for the original query file...\n"; 
  ifstream fq(qfile.c_str());
  if(!fq) {cerr<<"Open "<<qfile<< " error" << endl;}
  int op, id, dim;
  float qlx, qhx; 
  vector<float> qitem;
  while(fq) {
    if(!fq.good()) continue;
    qitem.clear();
    fq >> op >> id >> dim; 
    for(int i=0; i<dim; i++){
      fq >> qlx >> qhx; 
      qitem.push_back(qlx);
      qitem.push_back(qhx);
    } // for
    qlist.insert(make_pair(id, qitem));
  } // while(fq)
  fq.close();
} //setValidateQfile;

//Visit data function; 
void SpatialIndex::SimonVisitor::visitData(const IData& d){
  byte* data; 
  size_t len = 0;
  time_t time = clock();
  incrementPreresult();
  d.getData(len, &data);
  string s = reinterpret_cast<char*>(data);
  if(needValidate) {
    if(validate(s))
      incrementFinalResult();
  } else 
    incrementFinalResult(); 
  totalValidateTime += (double)(clock() - time)/CLOCKS_PER_SEC;
}
// The filtering process. 
bool SpatialIndex::SimonVisitor::validate(const string& s) {
  ifstream fparam("inva.txt");
  int op, id, dim; 
  if(!fparam) {cerr<<"File open error. "<<endl; return -1;}
  fparam >> dim >> dim; 
  float x; 
  gsl_matrix* A = gsl_matrix_alloc(dim, dim);
  gsl_matrix* W = gsl_matrix_calloc(dim, 2*(dim-2));
  gsl_vector * yl = gsl_vector_alloc (dim);
  gsl_vector * yh = gsl_vector_alloc (dim);
  gsl_vector * v = gsl_vector_calloc (dim);
  gsl_vector_set(v, dim-1, 1);
    
  // Get A^-1;
  for(int i=0; i<dim; i++){
    for(int j=0; j<dim; j++) {
      fparam >> x;
      gsl_matrix_set(A, i, j, x); 
    }
  }
  fparam.close();
  // Assigning yl and yh; 
  string item; 
  int i = 0; 
  string::const_iterator it = s.begin(); // s is constant;
  while (it != s.end()) {
    if((*it != ' ') && (*it != '\n') && (*it != EOF)) {
      item.push_back(*it); 
    } else {
      // start a new string operation; 
      if (i>2 && i%2==1) {
	// set yl;
	gsl_vector_set(yl, ((i-2)/2), atof(item.c_str()));
      } 
      if (i>2 && i%2==0) {
	// set yh;
	gsl_vector_set(yh, ((i-2)/2-1), atof(item.c_str()));
      } // if else -- word. 
      i++; 
      item.clear();
    } // if else -- char
    it++; 
  } // while -- data string; 
    // Buidling W; 
  int index=0;
  vector<float>::iterator itq = qlist[currentQuery].begin();
  while(itq != qlist[currentQuery].end()) {
    if (index % 2) {
      // Higher query boundary;
      gsl_matrix_set(W, (index-1)/2, index, 1);
      gsl_matrix_set(W, dim-2, index, -1 * (*itq));
    } else {
      // Lower query boundary; 
      gsl_matrix_set(W, index/2, index, -1);
      gsl_matrix_set(W, dim-2, index, *itq);
    }
    itq++;
    index++;
  } //while
    // Now here comes the final step. 
  gsl_vector * tmp = gsl_vector_calloc(dim);
  gsl_matrix * step1 = gsl_matrix_calloc(1, 2*(dim-2)); 
  gsl_matrix * ymat = gsl_matrix_calloc(1, dim);
  gsl_matrix * A1W = gsl_matrix_calloc(dim, 2*(dim-2));
  for(int i=0; i<dim; i++){
    gsl_matrix_set(ymat, 0, i, gsl_vector_get(yl, i));
  } 
  // part one; 
  gsl_blas_dgemm(CblasTrans, CblasNoTrans, 1.0, A, W, 0.0, A1W);
  time_t t_start = clock();
  gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, ymat, A1W, 0.0, step1);
  totalFilterTime += (double)(clock()-t_start)/CLOCKS_PER_SEC;
  // part two; 
  for(int i=0; i<dim; i++) {
    gsl_vector_set(tmp, i, gsl_matrix_get(A, dim-1, i));
  }
  double steptwo; 
  t_start = clock();
  gsl_blas_ddot(tmp, yl, &steptwo); 
  totalFilterTime += (double)(clock()-t_start)/CLOCKS_PER_SEC;
      
  bool passed = true;
  // Now, the final step. 
  for(int i=0; i<2*(dim-2); i++) {
    if (gsl_matrix_get(step1, 0, i) * steptwo > 0) {
      passed = false;
      break;
    }
  }
  gsl_matrix_free(A);
  gsl_matrix_free(W);
  gsl_matrix_free(ymat);
  gsl_matrix_free(A1W);
  gsl_vector_free(tmp);
  gsl_vector_free(yl);
  gsl_vector_free(yh);
  gsl_vector_free(v);
  return passed;  
}
