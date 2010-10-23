// -*- C -*-
/**
 * @file RequestMsg.h
 * @brief Definition of a RequestMsg class. 
 * @author Shumin Guo(guo.18@wright.edu)
 * @version 1.0.1
 */

#ifndef _REQUEST_MSG_H_
#define _REQUEST_MSG_H_

#include <string>
#include <iostream>
#include <vector>
#include <stdlib.h>

using namespace std;

/**
 * @brief The RequestMsg class defines a message type of request from
 * client. 
 */
class RequestMsg {
private:
  string cmd_;			/* command */
  int dim_; 			/* dimension. */
  vector<double> data_;		/* data. */

public:
   RequestMsg(); // Constructor
   RequestMsg(char *s);
   RequestMsg(string request);
   ~RequestMsg(){}

   string getCommand() const {return cmd_;}
   int getDimension() const {return dim_;}
   string getData() const {return "";}
   vector<double>& getDataValue(){return data_;}

   void setCommand(string m);
   void setDimension(string m);
   void setData(string m);
   
   void parseParam(string &param); 	/* param parser. */
};
#endif // ifdef
