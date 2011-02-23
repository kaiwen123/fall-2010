// Spatial Index Library
//
// Copyright (C) 2002 Navel Ltd.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  Email:
//    mhadji@gmail.com

// NOTE: Please read README.txt before browsing this code.

// include library header file.
#include <SpatialIndex.h>
#include <ctime>
using namespace SpatialIndex;
using namespace std;

#define INSERT 1
#define DELETE 0
#define QUERY 2

#include <assert.h>
#include <iostream>
#include <fstream>
#include <map>
#include <queue>
#include <cmath>

class MyVisitor : public IVisitor
{
private:
  int numNodeVisited;
  int totalResult; 
public:
  MyVisitor():numNodeVisited(0),totalResult(0){}

  void visitNode(const INode& n) {numNodeVisited++;}

  // Return the number of node that was visited; 
  int getNumNodeVisited() {
    return numNodeVisited; 
  }

  int getNumTotalResult() {
    return totalResult;
  }

  void visitData(const IData& d)
  {
    //cout << d.getIdentifier() << endl; 

    //if(!reinterpret_cast<RTree::Data&>(d).hasVisited()) {
    byte* data; 
    size_t len = 0;
    d.getData(len, &data);
    string s = reinterpret_cast<char*>(data);
    cout << s;
    //reinterpret_cast<RTree::Data&>(d).setVisited();  
    //}
    totalResult++;
  }

  void visitData(std::vector<const IData*>& v) {}
};

int main(int argc, char** argv)
{
  int totalVisit = 0; 
  try
    {
      if (argc != 5)
	{
	  cerr << "Usage: " << argv[0] << \
	    " input_file tree_file capacity " \
	    "query_type [intersection | 10NN | selfjoin]." << endl;

	  return -1;
	}

      uint32_t queryType = 0;

      if (strcmp(argv[4], "intersection") == 0) queryType = 0;
      else if (strcmp(argv[4], "10NN") == 0) queryType = 1;
      else if (strcmp(argv[4], "selfjoin") == 0) queryType = 2;
      else
	{
	  cerr << "Unknown query type." << endl;
	  return -1;
	}

      ifstream fin(argv[1]);
      if (! fin)
	{
	  cerr << "Cannot open data file " << argv[1] << "." << endl;
	  return -1;
	}

      // Create a new storage manager with the provided base name and
      // a 4K page size. 
      string baseName = argv[2];
      IStorageManager* diskfile = 
	StorageManager::createNewDiskStorageManager(baseName, 4096);

      StorageManager::IBuffer* file = 
	StorageManager::createNewRandomEvictionsBuffer(*diskfile, 10, false);

      id_type indexIdentifier;
       //std::cout << "a" << std::endl;
      size_t count = 0;
      id_type id;		// id of the item; 
      uint32_t op;		// operation code 0, 1, 2;
      uint32_t dim; 		// Dimension of data; 
      uint32_t total_result; 	// Total result; 
      double x, y; 		// (x, y) value of a dimension; 
      float total_sec = 0.0; 	// Total time used for query; 
      fin >> op >> id >> dim;
      MyVisitor vis;
      ISpatialIndex* tree = \
	RTree::createNewRTree(*file, 0.7, atoi(argv[3]), \
			      atoi(argv[3]), dim, \
			      SpatialIndex::RTree::RV_RSTAR,\
			      indexIdentifier);
      while (fin)
	{
	  double plow[dim], phigh[dim]; // Low and High point of the
					// region; 
	  if (! fin.good()) continue;	// skip newlines, etc.
	  for(int i = 0; i < dim; i++) {
	    fin >> plow[i] >> phigh[i]; 
	  }
	  if (op == INSERT)
	    {
	      Region r = Region(plow, phigh, dim);
#ifdef MYDEGUB
	      cout << "Inserting " << id << endl; 
	      cout << r << endl; 
#endif
	      // Insert the data string into the data node; 
	      // So that, next time, we can easily print it out. 
	      ostringstream os;
	      os << op << " " << id << " " << dim;
	      for(int x = 0; x < dim; x++){
		os << " " << plow[x] << " " << phigh[x];
	      }
	      os << "\n"; 

	      string data = os.str();
	      tree->insertData(data.size() + 1, \
			       reinterpret_cast<const byte*>(data.c_str()), \
			       r, id);
	    }
	  else if (op == DELETE)
	    {
	      Region r = Region(plow, phigh, dim);
	      if (tree->deleteData(r, id) == false)
		{
		  cerr << "******ERROR******" << endl;
		  cerr << "Cannot delete id: " << id \
		       << " , count: " << count << endl;
		  return -1;
		}
	    }
	  else if (op == QUERY)
	    {
	      if (queryType == 0)
		{
		  Region r = Region(plow, phigh, dim);
		  time_t t_start = clock(); // Start timing; 
		  tree->intersectsWithQuery(r, vis);
		  // Add each query time to the total; 
		  total_sec += (double)((clock()-t_start))/CLOCKS_PER_SEC;
		}
	      else if (queryType == 1)
		{
		  Point p = Point(plow, dim);
		  tree->nearestNeighborQuery(10, p, vis);
		  totalVisit += vis.getNumNodeVisited();
		}
	      else
		{
		  Region r = Region(plow, phigh, dim);
		  tree->selfJoinQuery(r, vis);
		}
	      
	    }

	  if ((count % 1000) == 0)
	  // cerr << "count = " << count << endl;
	  count++;
	  fin >> op >> id >> dim;
	}
      
      total_result = vis.getNumTotalResult();
      totalVisit = vis.getNumNodeVisited();
      cerr << " -RTree- " << total_sec << " "	  \
	   << total_result << " " << totalVisit << endl; 

      // cout << "Total Visits is: " << totalVisit << endl; 
      bool ret = tree->isIndexValid();
      if (ret == false) cerr << "ERROR: Structure is invalid!" << endl;
      //else cerr << "The structure seems O.K." << endl;

      delete tree;
      delete file;
      delete diskfile;
    }
  catch (Tools::Exception& e)
    {
      cerr << "******ERROR******" << endl;
      std::string s = e.what();
      cerr << s << endl;
      return -1;
    }
  catch (...)
    {
      cerr << "******ERROR******" << endl;
      cerr << "other exception" << endl;
      return -1;
    }

  return 0;
}
