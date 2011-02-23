#include <ctime>
#include <SpatialIndex.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_blas.h>
using namespace SpatialIndex;
using namespace std;

#include <assert.h>
#include <iostream>
#include <fstream>
#include <queue>
#include <cmath>
#include <ctime>
#include <string>
#include <map>
/*Visitor class*/
class MyVisitor : public IVisitor
{
private:
  int numNodeVisited;
  int totalResult;		// number of result for step two; 
  int preresult;                // number of result of step one; 
  int currentQuery; 		// Used as a key to search the original query file; 
  float filteringtime; 		// time of the filtering process; 
  string qfile;			// Orignal query file for validation;
  bool needValidate;
  float totalFilterTime;
  map<int, vector<float> > qlist;
public:
  MyVisitor():numNodeVisited(0),totalResult(0),needValidate(false),
	      preresult(0),totalFilterTime(0.0){}
  void visitData(std::vector<const IData*>& v) {}
  int getNumNodeVisited() { return numNodeVisited;}
  int getNumTotalResult() { return totalResult;}
  int getNumPreresult() {return preresult;}
  float getFilterTime() {return totalFilterTime;}
  void incrementPreresult() {preresult++;}
  void incrementFinalResult() {totalResult++;}
  void setCurrentQuery(int q) {currentQuery = q;}
  void visitNode(const INode& n) {numNodeVisited++;}

  void setValidateQfile(string& q) {
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
	//cout << qlx << " " << qhx << " "; 
	qitem.push_back(qlx);
	qitem.push_back(qhx);
      } // for
      //cout << endl; 
      qlist.insert(make_pair(id, qitem));
    } // while(fq)
    fq.close();
    // Test the map content. 
    // map<int, vector<float> >::iterator it = qlist.begin(); 
    // while(it != qlist.end()) {
    //   cout << it->first << " "; 
    //   // Test the content of the item; 
    //   vector<float>::iterator itv = /*(it->second)*/qlist[it->first].begin();
    //   while(itv != /*(it->second)*/qlist[it->first].end()) {
    // 	cout << *itv << " ";
    // 	itv++;
    //   } //while vector
    //   cout << endl; 
    //   it++;
    // }
    // while map  
  } //setValidateQfile;
  
  //Visit data function; 
  void visitData(const IData& d){
    byte* data; 
    size_t len = 0;
    incrementPreresult();
    d.getData(len, &data);
    //cout << "current query is : " << currentQuery << endl; 
    //cout << "total visit is : " << totalResult << endl; 
    string s = reinterpret_cast<char*>(data);
  if(needValidate) {
    time_t t_start = clock();
    if(validate(s))
      incrementFinalResult();
    totalFilterTime += (double)((clock()-t_start))/CLOCKS_PER_SEC;
  } else 
    incrementFinalResult();
  
  
}
// The filtering process. 
  bool validate(const string& s) {
    ifstream fparam("inva.txt");
    int op, id, dim; 
    //cout << "Current Query id is : " << currentQuery << endl; 
    if(!fparam) {cerr<<"File open error. "<<endl; return -1;}
    fparam >> dim >> dim; 
    float x; 
    //cout << "dim = " << dim << "  >>>>> " << __LINE__ << endl; 
    // Think about reading in inverse of A from file; 
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
	//cout << gsl_matrix_get(A, i, j) << " "; 
      }
      //cout << endl; 
    }
    fparam.close();
    // Assigning yl and yh; 
    //cout << "Data: " << s << endl;    
    string item; 
    int i = 0; 
    string::const_iterator it = s.begin(); // s is constant;
    while (it != s.end()) {
      if((*it != ' ') && (*it != '\n') && (*it != EOF)) {
	item.push_back(*it); 
	//cout << *it << " ";
      } else {
	// start a new string operation; 
	//cout << item << endl;
	if (i>2 && i%2==1) {
	  // set yl;
	  //cout << "yl: i = "<< i << " " << "(i-2)/2 = " << (i-2)/2 << " ";
	  gsl_vector_set(yl, ((i-2)/2), atof(item.c_str()));
	  // cout << "yl " << (i-2)/2 << " "			\
	  //      << gsl_vector_get(yl, ((i-2)/2)) << endl; 
	} 
	if (i>2 && i%2==0) {
	  // set yh;
	  //cout << "yh: i = "<< i << " " << (i-dim)/2 << " " << item << endl; 
	  gsl_vector_set(yh, ((i-2)/2-1), atof(item.c_str()));
	  // cout << "yh: " << (i-2)/2-1 << " "				\
	  //      << gsl_vector_get(yh, ((i-2)/2-1)) << endl; 
	} // if else -- word. 
	i++; 
	item.clear();
      } // if else -- char
      it++; 
    } // while -- data string; 
    //cout << "Successfully constructed A, yl, yh..." << __LINE__ << endl; 
    // test vector yl and yh; 
    // for(int i=0; i<dim; i++){
    //   cout << "yl = " << gsl_vector_get(yl, i) << endl;
    //   cout << "yh = " << gsl_vector_get(yh, i) << endl;
    // }

    // Buidling W; 
    int index=0;
    vector<float>::iterator itq = qlist[currentQuery].begin();
    while(itq != qlist[currentQuery].end()) {
      //cout << *itq << " ";
      if (index % 2) {
	// Higher query boundary;
	// cout << "High query : " << *itq << " dim = " << dim 
	//      << " index = " << index << endl; 
	gsl_matrix_set(W, (index-1)/2, index, 1);
	gsl_matrix_set(W, dim-2, index, -1 * (*itq));
	// gsl_vector_set(yh, index/2+1, *itq);
	// cout << "hl : " << gsl_vector_get(yh, index/2+1) << endl; 
      } else {
	// Lower query boundary; 
	// cout << "Low query : " << *itq << " dim = " << dim 
	//      <<" index = " << index << endl; 
	gsl_matrix_set(W, index/2, index, -1);
	gsl_matrix_set(W, dim-2, index, *itq);
	//gsl_vector_set(yl, index/2, *itq); 
	//cout << "hl : " << gsl_vector_get(yh, index/2+1) << endl;
      }
      itq++;
      index++;
    } //while
    // Test W. 
    // for (int i=0; i<dim; i++) {
    //   for(int j=0; j<2*(dim-2); j++) {
    //   	cout << gsl_matrix_get(W, i, j) << " ";
    //   }
    //   cout << endl; 
    // }

    //cout << "SUCCESSFULLY constructed A, yl, yh and W..." << __LINE__ << endl;  
    // Now here comes the final step. 
    gsl_vector * tmp = gsl_vector_calloc(dim);
    gsl_matrix * step1 = gsl_matrix_calloc(1, 2*(dim-2)); 
    gsl_matrix * ymat = gsl_matrix_calloc(1, dim);
    gsl_matrix * A1W = gsl_matrix_calloc(dim, 2*(dim-2));
    for(int i=0; i<dim; i++){
      //cout << "i = " << i; 
      gsl_matrix_set(ymat, 0, i, gsl_vector_get(yl, i));
      //cout << " ymat: " << gsl_matrix_get(ymat, 0, i) << endl; 
    } 
    // part one; 
    gsl_blas_dgemm(CblasTrans, CblasNoTrans, 1.0, A, W, 0.0, A1W);
    gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, ymat, A1W, 0.0, step1);
    // cout << "step one: = ";
    // for(int i=0; i<2*(dim-2); i++) {
    // 	cout << gsl_matrix_get(step1, 0, i) << " "; 
    //}
    // part two; 
    for(int i=0; i<dim; i++) {
      gsl_vector_set(tmp, i, gsl_matrix_get(A, dim-1, i));
      //cout << "v^T * A^-1 = " << gsl_vector_get(tmp, i) << endl; 
      //cout << "yl = " << gsl_vector_get(yl, i) << endl;  
    }
    double steptwo; 
    //cout << endl << "step two = ";
    gsl_blas_ddot(tmp, yl, &steptwo); 
    //cout << steptwo << endl << endl; 
      
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
};

// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
int main(int argc, char** argv)
{
  try
    {
      MyVisitor vis;
      if (argc == 5) {
	// Five parameter will combine the two steps;
	string q = string(argv[4]);
	vis.setValidateQfile(q);
      } else if (argc != 4) {
	cerr << "Usage: " << argv[0] <<	      \
	  " data_file query_file capacity <orig_qfile(optional)>" << endl;
	return -1;
      }

      ifstream find(argv[1]);
      ifstream finq(argv[2]);
      if (! (find && finq)){
	cerr << "Cannot open file " << argv[1] << " or " << argv[2] << endl;
	return -1;
      }

      string baseName = argv[1];
      IStorageManager* diskfile = 
	StorageManager::createNewDiskStorageManager(baseName, 4096);

      StorageManager::IBuffer* file = 
	StorageManager::createNewRandomEvictionsBuffer(*diskfile, 10, false);

      id_type indexIdentifier;
      size_t count = 0;
      id_type id;		// id of the item; 
      uint32_t op;		// operation code 0, 1, 2;
      uint32_t dim; 		// Dimension of data; 
      double x, y; 		// (x, y) value of a dimension; 
      float total_sec = 0.0; 	// Total time used for query; 
      int exhaustResults = 0; 
      int exhaustHits = 0; 
      double total_exhaust_sec = 0.0;
       
      vector<vector<float> > dlist;
      vector<float> ditem;
 
      find >> op >> id >> dim;
      
      ISpatialIndex* tree = \
	RTree::createNewRTree(*file, 0.7, atoi(argv[3]), \
			      atoi(argv[3]), dim, \
			      SpatialIndex::RTree::RV_RSTAR,\
			      indexIdentifier);
      cout << "Building RTree ... ... " << endl; 
      while (find) {
	double plow[dim], phigh[dim]; // Low and High point of the
	// region; 
	if (! find.good()) continue;	// skip newlines, etc.
	for(int i = 0; i < dim; i++) {
	  find >> plow[i] >> phigh[i]; 

	  ditem.push_back(plow[i]);
	  ditem.push_back(phigh[i]);
	  //cout << ditem.at(i*2) << " " << ditem.at(i*2+1) << endl;
	}
	//cout << endl;
	dlist.push_back(ditem);
	ditem.clear();
	Region r = Region(plow, phigh, dim);
	
	// Insert the data string into the data node; 
	// So that, next time, we can easily print it out. 
	ostringstream os;
	os << op << " " << id << " " << dim;
	for(int x = 0; x < dim; x++){
	  os << " " << plow[x] << " " << phigh[x];
	}
	os << "\n"; 
	
	string data = os.str();
	tree->insertData(data.size() + 1,				
			 reinterpret_cast<const byte*>(data.c_str()),
			 r, id);
	find >> op >> id >> dim;
      }
      
      cout << "Querying ..." << endl; 
      while (finq) {
	finq >> op >> id >> dim;
	double plow[dim], phigh[dim]; // Low and High point of the
	// region; 
	if (! finq.good()) continue;	// skip newlines, etc.
	//cout << "Query : " ;
	for(int i = 0; i < dim; i++) {
	  finq >> plow[i] >> phigh[i]; 
	  //cout << plow[i] << " " << phigh[i] << " ";
	}
	//cout << endl; 
	// Linear Scan; 
	vector<vector<float> >::iterator itl = dlist.begin(); 
	while(itl != dlist.end()) {
	  exhaustHits++; 
	  //vector<float>::iterator iti = itl->begin();
	  bool passed = true; 
	  int i = 0;
	  //cout << "Data: "; 
	  time_t start_e = clock();
	  for(int i=0; i<dim; i++) {
	    float dlow = itl->at(2*i), dhigh = itl->at(2*i+1);
	    if((dlow < plow[i]) || (dhigh > phigh[i])){
	      passed = false;
	      break; 
	    }
	  }
	  total_exhaust_sec += (double)((clock()-start_e))/CLOCKS_PER_SEC;
	  if(passed)
	    exhaustResults++; 
	  itl++;	  
	}// while - data list; 

	Region r = Region(plow, phigh, dim);
	vis.setCurrentQuery(id);

	time_t t_start = clock(); // Start timing; 
	tree->intersectsWithQuery(r, vis);
	total_sec += (double)((clock()-t_start))/CLOCKS_PER_SEC;
      }
  
      cerr << argv[1] << " -RTree- " << total_sec << " " 
	   << vis.getNumPreresult() << " "
	   << vis.getNumNodeVisited() << endl; 
      cerr << argv[1] << " -Exhaustive- " << total_exhaust_sec << " " 
	   << exhaustResults << " "
	   << exhaustHits << endl;
      
      if(argc == 5) 
	cerr << argv[1] << " -filter- " << vis.getFilterTime() << " "
	     <<vis.getNumTotalResult() << " " 
	     << vis.getNumPreresult() << endl;

      bool ret = tree->isIndexValid();
      if (ret == false) cerr << "ERROR: Structure is invalid!" << endl;
      //else cerr << "The structure seems O.K." << endl;
      delete tree;
      delete file;
      delete diskfile;
    }
  catch (...)
    {
      cerr << "******ERROR******" << endl;
      cerr << "other exception" << endl;
      return -1;
    }
  return 0;
}
