#include <SpatialIndex.h>
#include "SimonVisitor.cc"
using namespace SpatialIndex;
using namespace std;


// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
int main(int argc, char** argv)
{
  try
    {
      //MyVisitor vis;
      SimonVisitor vis;
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
      int exhaustResults = 0; 
      int exhaustHits = 0; 
      double total_exhaust_sec = 0.0;
      double totalQueryTime = 0.0;
       
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
	time_t t_start = clock();
	tree->intersectsWithQuery(r, vis);
	totalQueryTime += (double)(clock()-t_start)/CLOCKS_PER_SEC;
      }
  
      cerr << argv[1] << " -RTree- " << totalQueryTime-vis.getValidateTime() << " " 
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
