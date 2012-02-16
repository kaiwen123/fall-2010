#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <signal.h>
#include <rpc/rpc.h>
#include <rpc/pmap_clnt.h>
#include <errno.h>
#include <assert.h>

#include "server.h"		/* generated by rpcgen */

int gethostname (char *name, size_t len);
int pmap_unset (unsigned long, unsigned long); /* unregister with OS */

struct sigaction
{
  void (*sa_handler) (int);
  void (*sa_sigaction) (int, void *, void *);
  sigset_t sa_mask;
  int sa_flags;
  void (*sa_restorer) (void);
};
int sigaction (int, const struct sigaction *, struct sigaction *);
int sigemptyset (sigset_t * set);
int kill (pid_t pid, int sig);

void *callbackfromwbs_1 (OneLn * argp, CLIENT * clnt);

/* in xwindow.c */
long atoir (char *p, int r);
int openxwindow (char *, char *);
void drawline (void *);
void closexwindow ();
int trackpointer (void *, int);

/* -eof- */