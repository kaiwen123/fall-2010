// -*- C++ -*-
/**
 *
 * @file main.cpp 
 * @brief This is the main program for the socket server to implement
 * the client/server data store architecture. 
 *
 * This program will be responsible for the following things:
 * 1, Start up a socket server on a given ip/port pair. 
 * 2, Accept socket connections and do related work according to the
 * client commands and parameters. 
 * 3, This program will work as a daemon process. 
 * 4, This program is able to accept signals. 
 *
 */ 
/*
** server.c -- a stream socket server demo
*/

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/wait.h>
#include <signal.h>
#include "RequestMsg.h"
#include "ResponseMsg.h" 

using namespace std;

#define PORT "10001"   // the port users will be connecting to
#define BACKLOG 10     // how many pending connections queue will hold
#define BUFSIZE 1000   // Size of receive/send buffer. 
void sigchld_handler(int s)
{
  while(waitpid(-1, NULL, WNOHANG) > 0);
}

// get sockaddr, IPv4 or IPv6:
void *get_in_addr(struct sockaddr *sa)
{
  if (sa->sa_family == AF_INET) {
    return &(((struct sockaddr_in*)sa)->sin_addr);
  }

  return &(((struct sockaddr_in6*)sa)->sin6_addr);
}

int main(void)
{
  int sockfd, new_fd;  // listen on sock_fd, new connection on new_fd
  struct addrinfo hints, *servinfo, *p;
  struct sockaddr_storage their_addr; // connector's address information
  socklen_t sin_size;
  struct sigaction sa;
  int yes=1;
  char s[INET6_ADDRSTRLEN];
  int rv;

  memset(&hints, 0, sizeof hints);
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE; // use my IP

  if ((rv = getaddrinfo(NULL, PORT, &hints, &servinfo)) != 0) {
    fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(rv));
    return 1;
  }

  // loop through all the results and bind to the first we can
  for(p = servinfo; p != NULL; p = p->ai_next) {
    if ((sockfd = socket(p->ai_family, p->ai_socktype,
			 p->ai_protocol)) == -1) {
      perror("server: socket");
      continue;
    }

    if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes,
		   sizeof(int)) == -1) {
      perror("setsockopt");
      exit(1);
    }
    if (bind(sockfd, p->ai_addr, p->ai_addrlen) == -1) {
      close(sockfd);
      perror("server: bind");
      continue;
    }
    break;
  }
  if (p == NULL){
    fprintf(stderr, "server: failed to bind\n"); 
    return 2;
  }
  freeaddrinfo(servinfo); // all done with this structure
  
  if (listen(sockfd, BACKLOG) == -1) {
    perror("listen");
    exit(1);
  }

  sa.sa_handler = sigchld_handler; // reap all dead processes
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_RESTART;
  if (sigaction(SIGCHLD, &sa, NULL) == -1) {
    perror("sigaction");
    exit(1);
  }

  printf("server: waiting for connections...\n");

  while(1) {  // main accept() loop
    sin_size = sizeof their_addr;
    new_fd = accept(sockfd, (struct sockaddr *)&their_addr, &sin_size);
    if (new_fd == -1) {
      perror("accept");
      continue;
    }

    // If you want to show client's address, it is here. 
#ifdef SHOW_SRC
    printf("Hi there, from  %s#\n",inet_ntoa(pin.their_addr));
    printf("Coming from port %d\n",ntohs(pin.sin_port));
#endif

    inet_ntop(their_addr.ss_family,
	      get_in_addr((struct sockaddr *)&their_addr),
	      s, sizeof s);
    printf("server: got connection from %s\n", s);

    // if (!fork()) { // this is the child process
    //   close(sockfd); // child doesn't need the listener
    //   if (send(new_fd, "Hello, world!", 13, 0) == -1)
    // 	perror("send");
    //   close(new_fd);
    //   exit(0);
    // }
    // close(new_fd);  // parent doesn't need this

    char buf[BUFSIZE]; 
    bzero(buf, BUFSIZE);
    int n = read(new_fd, buf, BUFSIZE);
    if(n < 0) perror("Error reading from socket. "); 
    //printf("%s\n", buf);

    RequestMsg request(buf);
    cout << "Server receive " << request.getCommand() 
	 << " request from client." << endl; 
    bzero(buf, BUFSIZE);    
    // Reading data from client. 
    write(new_fd, "SUCCESS", 7);
    close(new_fd);
    sleep(1);
    // Write data to client. 

  }

  return 0;
}

