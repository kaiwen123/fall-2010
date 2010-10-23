// -*- C -*-
/**
 * @file ResponseMsg.h
 * @brief Definition of a ResponseMsg class. 
 * @author Shumin Guo(guo.18@wright.edu)
 * @version 1.0.1
 */

#ifndef _RESPONSE_MSG_H_
#define _RESPONSE_MSG_H_

#include <string>
#include <iostream>
#include <vector>
#include <stdlib.h>

using namespace std;

/**
 * @brief The ResponseMsg class defines a message type of request from
 * client. 
 */
class ResponseMsg {
private:
  string cmd_;			/* command */
  int dim_; 			/* dimension. */
  vector<double> data_;		/* data. */

public:
   ResponseMsg(); // Constructor
   ResponseMsg(char *s);
   ~ResponseMsg(){}
   
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
