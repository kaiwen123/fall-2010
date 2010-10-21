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

#include "Message.h"

using namespace std;

/**
 * @brief The RequestMsg class defines a message type of request from
 * client. 
 */
class RequestMsg : public Message {
private:
  string cmd_;			/* command */
  int dim_; 			/* dimension. */
  vector<double> data_;		/* data. */

public:
   RequestMsg(); // Constructor
   RequestMsg(char *s);
   virtual ~RequestMsg(){}

   virtual string getCommand() const {return cmd_;}
   virtual int getDimension() const {return dim_;}
   virtual string getData() const {return "";}
   vector<double>& getDataValue(){return data_;}

   virtual void setCommand(string m);
   virtual void setDimension(string m);
   virtual void setData(string m);

   void parseParam(string &param); 	/* param parser. */
};
#endif // ifdef
