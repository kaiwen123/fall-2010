// -*- C -*-
/**
 * @file DataSet.h
 * @brief Definition of a ataSet class. 
 * @author Shumin Guo (guo.18@wright.edu)
 * @version 1.0.0
 */
// $Log$

#ifndef _DataSetClass_
#define _DataSetClass_ 
#include <iostream>
#include <fstream>
#include "defs.h"
#include "HashTree.h"
#include <map>

using namespace std; 
class DataSetLessThan;
class DataSet {
 private: 
  int gnum;			/* number of genes. */
  int tnum;			/* number of transactions. */
  vector<vector<Item> > d_sets; /* Feature sets. */
  map<int, int> fst;		/* first level item sets. */
  map<Itemset, int/* , DataSetLessThan */> snd; /* Second level item sets. */
  /* map<string, pair<Itemset, int> > snd;	/\* Second level item sets. *\/ */
  /* 	if(!snd[key]){ */
  /* 	  pair<Itemset, int> cnt(set1, 1); */
  /* 	  snd[key].second = cnt; */
  /* 	} else { */
  /* 	  snd[key].second++; */
  /* 	} */
 public:
  DataSet(){}
 DataSet(int g, int t):gnum(g),tnum(t){} /* Default constructor. */
  ~DataSet(){}

  /* getters. */
  int getNumGenes() const {return gnum;}
  int getNumTrans() const {return tnum;}

  /* Setters. */
  void setNumGenes(int g) {gnum = g;}
  void setNumTrans(int t) {tnum = t;}

  /* Scanners. */
  void scanLevelOne(int id);
  void printLevelOne();
  void scanLevelTwo();
  void printLevelTwo();

  /**
   * @brief Load gene data from file.
   * @param function will prompt user to enter the file name. 
   * @return true on success and false on failure. 
   */
  bool loadFromFile(string fname);

  /**
   * @brief Do the gene Item mapping from group data into unique ids.
   * @param none.
   * @return true on success and false on failure. 
   */
  bool doItemMap(); 

  /**
   * @brief Save item map into given file. 
   * @param fname file to save mapping result to.
   * @return true on success and false on failure. 
   */
  bool saveItemMap(string fname);

  /**
   * @brief Do apriori association rule mining to gene data set.
   * @param none.
   * @return true on success and false on failure. 
   */
  bool doApriori();

  /**
   * @brief Save frequent itemsets into given file.
   * @param fname The name of file to save to.
   * @return true on success and false on failure. 
   */
  bool saveFreqItemSets(string fname);

  /**
   * @brief Insert data into gene vector. 
   * @param row_num row number of inserted item.
   * @param col_num column number of inserted item.
   * @param str item information.
   * @return true on success and false on failure. 
   */
  bool insertData(int row_num, int col_num, string str); 

  /**
   * @brief Overloading the << operator to output stream. 
   * @param out output stream. 
   * @param g GeneDataSet object. 
   * @output out output stream. 
   */
  friend ostream& operator<<(ostream& out, DataSet& g);

  /**
   * @brief Find k genes with highest information gain after the
   * entropy based discretization.
   * @param k the number of gene to be searched. 
   * @return none.
   */
  void findTopkGene(int k); 
  void printInfoGain();
};

/* class DataSetLessThan { */
/*  public: */
/*   bool operator()(const Itemset& set1, const Itemset& set2) const { */
/*     if(set1.getSize() != set2.getSize()) return false; */
/*     bool result = true; */
/*     for(int i = 0; i < set1.getSize(); i++) { */
/*       result = result && (set1.getSet()[i] < set2.getSet()[i]); */
/*       //cout << set1.getSet()[i] << " " << set2.getSet()[i] << endl; */
/*       //if(set1.getSet()[i] >= set2.getSet()[i]) return false; */
/*     } */
/*     cout << set1 << " < " << set2 << " " << result << endl; */
/*     return result; */
/*   } */
/* }; */

#endif //ifdef
