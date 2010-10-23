#include "RequestMsg.h"
#include <boost/algorithm/string.hpp>

using namespace boost;
// Constructor. Zero out the strings.
RequestMsg::RequestMsg() {
}


// Request constructor. 
RequestMsg::RequestMsg(string request) {
  cout << request << endl; 
}

// Constructor to build a request msg from a c raw string. 
RequestMsg::RequestMsg(char *s) {
  // parsing the char string into command, param. 
  string str(s);
  int pos; 
  trim(str);
  pos = str.find_first_of(" ");
  cout << "Request info is: " << str << endl;
  cout << "Parsing......" << endl; 
  // parsing command. 
  if(!pos) cmd_ = str; 		// only command; 
  if(pos > 0) {
    cmd_ = str.substr(0, pos);	// command. 
    cout << "Command is: " << cmd_ << endl;
    str = str.substr(pos+1, str.size() - pos); 
    parseParam(str);
  }
}

// help function for parameter parsing.
// parsing parameters. with the form: 
// [dim d00 d01 d10 d11 ......]
void RequestMsg::parseParam(string &param) {
  // cout << "Parameters : " << str << endl; 
  char** p;
  double t_dat; 
  int pos;
  pos = param.find_first_of(" ");
  try {
    dim_ = atoi(param.substr(0, pos).c_str());
    cout << "Dimension is: " << dim_ << endl; 
    param = param.substr(pos+1, param.size() - pos);
  } catch(...) {
    cerr << "Error while parsing dimension." << endl;
  }

  // now, data parsing. 
  do {
    pos = param.find_first_of(" ");
    t_dat = strtod(param.substr(0, pos).c_str(), p);
    param = param.substr(pos+1, param.size()-pos).c_str();
    cout << "Data : " << t_dat << endl;
    data_.push_back(t_dat);
  } while(pos > 0);
}

// set up command. 
void RequestMsg::setCommand(string in) {
}
  
// set up dimension info. 
void RequestMsg::setDimension(string in) {
}
   
// Set up data.
void RequestMsg::setData(string in) {
}
