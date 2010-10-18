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

#include "Message.h"

using namespace std;

/**
 * @brief The ResponseMsg class defines a message type of request from
 * client. 
 */
class ResponseMsg : public Message {
private:
  string cmd_;			/* command */
  int dim_; 			/* dimension. */
  vector<double> data_;		/* data. */

public:
   ResponseMsg(); // Constructor
   ResponseMsg(char *s);
   virtual ~ResponseMsg(){}

   virtual string getCommand() const;
   virtual string getDimension() const;
   virtual string getData() const;

   virtual void setCommand(string m);
   virtual void setDimension(string m);
   virtual void setData(string m);

   void parseParam(string &param); 	/* param parser. */
};
#endif // ifdef
