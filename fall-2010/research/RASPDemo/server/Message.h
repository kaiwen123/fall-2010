// -*- C -*-
/**
 * @file Message.h
 * @brief Definition of a message class. 
 *
 */

#ifndef _MESSAGE_H_
#define _MESSAGE_H_

#include <string>

using namespace std;

/**
 * @brief The Message is a message class sent from a client to
 * the server node.  It indicates to the server node which command the 
 * client wants to execute and the first and second parameters that
 * were entered.  The command and parameters are char array of a fixed 
 * size so that the struct can be passed directly over a socket and be
 * read by the other process without sending any prior information  
 * regarding the size of the incoming message.  Functions to
 * manipulate the char arrays via strings are provided in order to
 * make them easier to work with.
 */
class Message {
private:
  char msg[1000];		/* message content. */

public:
  Message(){} // Constructor
   virtual ~Message(){}

   /**
    * @brief A group of getter functions.
    * These functions are responsible for returning commands,
    * dimension, and data to caller. 
    */
   virtual string getCommand() const = 0;
   virtual int getDimension() const = 0;
   virtual string getData() const = 0;
   
   /**
    * @brief A group of setter functions.
    * These group of functions are responsible for building up
    * commands. 
    */
   virtual void setCommand(string m) = 0;
   virtual void setDimension(string m) = 0;
   virtual void setData(string m) = 0;
};
#endif // ifdef
