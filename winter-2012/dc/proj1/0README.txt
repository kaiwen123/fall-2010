
A White Board Project using ONR RPC (SunRPC)

Prabhaker Mateti
pmateti@wright.edu

This 0README.txt is a supplement to the lectures.  It describes a bit
of the philosophy behind the course and some details of the source
code given to you as a start.

It is expected that you will study the source code closely.  It is
only 800+ lines.  You should become so familiar with it *as if* you
wrote it.  The only exception is xwindow.c; just read the comments.

Students whose Unix OS programming skills are rusty should study
examples much simpler than what you see in our project files for
{alarm, fork, pipes, signals, bash scripting}.  At WSU, we expect CEG
433/633 to have taught these.  Recommended book: W. Richard Stevens
and Stephen A. Rago, Advanced Programming in the Unix Environment,
Addison-Wesley, ISBN: 0-201-56317-7. http://www.apuebook.com/

Some of you worry this is C and not C++.  C is a subset of C++.  The
code does use lower-than-what-is-typical-in-C++ libraries.  This is
part of the goal of this course: Give you a good feel for the basics
of systems programming that is relevant to distributed computing.

RPC background is not expected.  Several of the RPC lib procedures
will be discussed in class.  On Linux, these have become part of C std
lib; on other OS you may have to explicitly give -lrpcsvc to the
linker (gcc).

1. Files
--------

The source code as given is ready to be built using make.  There
should be no compilation/link warnings except possibly in
rpcgen-erated code.  It should also run without any glitches.  If
either of these are not working out (i) in the OSIS Lab, report
problems to me, (ii) on Linux systems elsewhere, I may be able to
help, but there is too much ground to cover.

1.1 The following are the "original" files (i.e., files created by a
  human programmer).

Makefile	the makefile for the project
server.x	server rpc interface
server.c	server specific code
client.x	client rpc interface
client.c	client specific code
ed-script	to generate client_s.c from client_svc.c
xwindow.c	a collection of simple X11 procedures
transient.c	gets a vacant program number

1.2 "rpcgen server.x" generates:

server.h
server_xdr.c
server_clnt.c
server_svc.c

1.3 "rpcgen client.x" generates:

client.h
client_xdr.c
client_svc.c
client_clnt.c

1.4 client_s.c is a minor variation of client_svc.c mechanically
produced by running an editor script on client_svc.c.  We insert just
above the "svcrun()" line "startclient(...)".

1.5 Compilation

Keep the flags -Wall -ansi -pedantic -std=c99 on; read-up on what
these do.  There should be zero errors/ warnings.  The rpcgen
generated code unfortunately is not as kosher.  Note also that
non-Linux OS may have ancient rpcgens that generate even worse code so
far as these flags are concerned.

Our (simple minded) use of signals requires -D_BSD_SIGNALS.

2. How to Run It
----------------

(May be you should read Section 5. Miscellaneous now, and then return
to this section.)

2.1 Run the "server" as a background process on a machine of your choice.

  % server730 &

2.2 Run the "client" from one or more machines of your choice.  The
client needs four arguments:

  % client730 <srv-mchn-nm> <wb-nm> <wb-disp> <color>

<srv-mchn-nm> is the name of the machine running your server
<wb-nm>       is the name of the whiteboard you wish to create/join
<wb-disp>     is the name of the machine you are logged in for the X11 display
<color>	      is an X11 RGB 6-hex-digit number for lines of this client

The host names above can be IP addresses (better/ simpler actually).

In the source code as given: The server is one process running on host
H1.  The client consists of two processes both running on host H2.  It
is ok, but not desirable that H1 == H2.  H2 is where the X11 server is
also running, using the bash-equaivalent of "export DISPLAY=H2:0.0"

2.3 A simple "smoke test" with all the processes running on localhost:

smokeTest730() {
  ./server730 &
  echo -n Servert started, press Return to continue; read
  ./client730 localhost b0 localhost ff0000 & # RGB=ff0000 = red
  echo -n client started, press Return to continue; read
  ./client730 localhost b0 localhost ff00 & # RGB=00ff00 = green
  echo -n client started; press Return to continue; read
  ./client730 localhost b0 localhost ff & # RGB=0000ff = blue
  echo -n client started, press Return to continue; read
  rpcinfo -p
}

2.4 You will find the following useful.

killall730() {
  killall -q server730 client730
  ./deregall730 536871065  # change this to your server num
  ./deregall730
  rpcinfo -p
}

freshStart730() {
  killall730
  smokeTest730
  ps
  rpcinfo -p
}

The results of the last 'ps; rpcinfo -p' should show none of our RPC
processes.

3. What to submit
-----------------

Using the turnin program in the OSIS Lab:

/home/ceg730/ceg73000/bin/turnin P1 fileNames

Submit files with your changes clearly marked/highlighted.  Submit
*your* ReadMe.txt, not my 0README.txt.

4. Demo
-------

When you are done you must give me a demo of your program.  You will
get just one chance.  It is over in about 5 minutes, typically.  I may
ask you to rebuild the program prior to the demo, but have it
ready-to-run.  We expect some of our class students as audience.

Here is the acceptance test
http://www.cs.wright.edu/~pmateti/Courses/730/Projects/accept-test.html.

5. Miscellaneous
----------------

5.1 Locate where 'rpcinfo' is.  Perhaps it is in /usr/sbin instead of
    /usr/bin.  Read its man page.  Here is an example invocation on
    Linux machines,

% rpcinfo -p
   program vers proto   port
    100000    2   tcp    111  portmapper
    100000    2   udp    111  portmapper
    ... more lines

Do the above after a fresh reboot.  This shows the presence of other
standard RPC processes.  After you "close" your WB project demo, the
list then should be the same.

5.2 The standard make will compile the source code file deregall.c into a
  program named deregall730.  This can de-register transient RPC
  processes.

5.3 read: man killall; killall server730 client730.  A related program
    is pkill.

5.4 The xwindow.c can be compiled as a stand-alone program xwindow730
    to check for X11 problems.

5.5 Note the use of static int i within procedures.  This makes i
    "stay/live" even after that procedure returns; such an i is not
    allocated on the stack.

5.6 Linux Setup as needed by 730.  Some of you may wish to use your
    own Linux boxes for 730 work.  But Linux installations
    out-of-the-box are typically not "open" enough for 730 projects.
    Read through the thread devoted to this topic on our Discussion
    Board.

5.7 C vs C++.  If you know C++ well enough, you should be able to
    fully understand C.

C			C++		concept
malloc/free		new/delete	mem allocation
static			private		scope
static			static		life time
e1? e2 : e3		e1? e2 : e3	conditional expression

5.8 http://www.gnu.org/software/libc/manual/html_node/Backtraces.html
    Learn to use backtrace; it will offset the time you may otherwise
    waste in debugging.

5.9 See what libraries are actually in use: ldd client730 server730

# -eof-
