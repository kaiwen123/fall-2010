
/**
@mainpage CEG 730 Java WhiteBoard Project

@author Prabhaker Mateti pmateti@wright.edu http://www.cs.wright.edu/~pmateti

1 Overview of Whiteboard project in Java

This project implements the Whiteboard in Java language using RMI, but
leaves out a few pieces for you TODO.  It should work equally well on
both Linux and Windows.  The overall behavior is similar to the
730-C-RPC Whiteboard.

2. Files

<pre>
   java730.sh		// bash file, to be sources 
   Invoke.java		// support methods 
   LineCoords.java	// public data structure for a line 
   LinesFrame.java	// our little window of lines 
   LinesFrameImpl.java	// implementation of window of lines 
   WbAdmin.java         // interface of wbadministrator 
   WbServer.java	// interface of WhiteBoard server 
   WbClient.java	// interface of WhiteBoard client 
   WbClientImpl.java	// implementation of WhiteBoard client 
   WbServerImpl.java	// implementation of WhiteBoard server 
</pre>

3. How to Build

Make sure your CLASSPATH includes . and ..  .  Study the java730.sh file.

Make sure you are invoking the proper javac and java.  On OSIS Lab
systems, there are multiple versions of JDK installed.

<pre>
% which java		    # check where your java JVM is located
% java -version
</pre>

You can call wb730build defined in java730.sh.  This is the simplest,
as javac subsumes some of the Makefile features.  Before invoking it,
make sure you are in the parent dir of WhiteBoard/*.java files.

4.  How to Run

<pre>
% cat > ~/java.policy << END
grant {
    permission java.net.SocketPermission "*:1024-65535", "connect,accept";
    permission java.net.SocketPermission "*:80", "connect";
};
END
</pre>

There are several ways of running this program. wb730run defined in
java730.sh is one such example that starts one server and four
clients.  Before run, do wb730rmi.  'killall rmiregistry java' kills
all processes whose names are given.

For rmiregistry: If port 1099 is in use already, you need to make
corresponding edits in Invoke.java.  Server will print its URL, e.g.,
//localhost/S1.

In general, to start a client do:

<pre>
% java WhiteBoard.WbClientImpl <idn> <boardnm> <displayMcnm> <URL-of-server>
</pre>

The idn is an arbitrary non-neg integer used as a pseudo process id.
An example URL for the server is //localhost/S1.  The client will
start a LinesFrame.

Or you may invoke these from WbAdmin:

<pre>
% java WhiteBoard.WbAdmin
</pre>

The disadvantage here is the stdout is then not seen.

You may find the pj alias  defined in java730.sh useful.


5. Subtleties


pleaseDie(): If we *really* die in a remote method, the caller will
get an exception.  So, we make sure the method returns normally, and
die a little later.

addClient() occurs in recvDisplayObj()

Window Expose(s): I could not find what the expose event is called
in Java.  So did a work around using 
<tt>
public void windowActivated(WindowEvent e)
</tt>

So to see lines of a window that was just uncovered, click on the
title bar.

You may also find that occasionally some mouse clicks are "lost".

I do not think it is worth our time to properly fix these.

-eof- 

*/
