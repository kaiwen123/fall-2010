// file: WbAdmin.java by pmateti@wright.edu
// This is a "shell" for the WhiteBoard project of CEG 730

package WhiteBoard;

import java.io.*;
import java.util.*;
import java.rmi.*;

public class WbAdmin extends java.rmi.server.UnicastRemoteObject{
    private String serverURL, adminNm, myURL; 
    private WbServer myserver;
    private Vector vServers;	// store all the connected servers. 

    private static final String menu = "\nWbAdmin: create a " +
	"[s]erver, [a]dd client, [q]uery, [t]ransfer, e[x]it";

    // constructor. 
    public WbAdmin(String[] args) throws Exception {
	// args = [clientId, wbserverURL]
	super(); 
	vServers = new Vector();
	myURL = Invoke.makeURL('C', args[0]); 
	Naming.rebind(myURL, this); 
	Invoke.myPrint("WbAdmin", "did Naming.rebind" + myURL); 

	adminNm = java.net.InetAddress.getLocalHost().getHostName(); 
	serverURL = args[1]; 
	myserver = (WbServer) Naming.lookup(serverURL); 
	vServers.addElement(myserver);
	Invoke.myPrint("WbAdmin waiting for ", serverURL + " to reply.");
    }

    /**
     * Request server to add a new client. 
     */
    private void addClientReq() {
	// args should be = [clientId, brdNm,displayMcnm,wbserverURL,color].
	String args = Invoke.promptAndGet("clientId brdNm DisplayOn ServerURL color");
	args = "java WhiteBoard.WbClientImpl " + args;
	Runtime.getRuntime.exec(args); 
	return true;
    }

    // Transfer a white board to a new server. 
    private void transferReq() {
	String argstr = Invoke.promptAndGet("OldServerURL boardnm NewServerURL");
	String[] args = argstr.split(String(" "));
	WbServer wbs = (WbServer) Naming.lookup(args[0]); 

	try { vServers.transfer(args[1], args[2]); }
	catch (Exception e) { e.printStackTrace(); }
    }

    // Query for inforamtion from each server. 
    private void queryReq() {
	String args = Invoke.promptAndGet("Query Server URL: ");
	WbServer wbs = (WbServer) Naming.lookup(args); 
	// vServers.addElement(wbserver);
	
	wbs = wbs.query(); 
	int brdcnt = 0, clntcnt, lncnt; 
	for(Enumeration es = wbs.vBoards.elements(); es.hasMoreElements(); ) {
	    System.out.println("Board: " + es.boardName); 
	    clntcnt = 0; lncnt = 0; 
	    // clients of board. 
	    for(Enumeration e = es.vClients.elements(); e.hasMoreElements(); ) {
		System.out.println("Machine:   " + e.thisMcnm + 
				   "Board Name:" + e.myBoardNm +
				   "Client URL:" + e.myURL +
				   "Server URL:" + e.myServerURL);
		clntcnt++; 
	    }
	    System.out.println("Total Clients: " + Integer.toString(clntcnt)); 
	    // lines of board.
	    for(Enumeration e = es.vLines.elements(); e.hasMoreElements(); ) {
		System.out.println("x0: " + Integer.toString(e.x0) + 
				   "y0: " + Integer.toString(e.y0) +
				   "x1: " + Integer.toString(e.x1) +
				   "y1: " + Integer.toString(e.y1)); 
		lncnt++; 
	    }
	    System.out.println("Total Lines: " + Integer.toString(lncnt)); 
	}
    }

    private void serverCreate() {
	String args = promptAndGet("OldServerURL NewServerURL: "); 
	String [] arglist = args.split(" "); 
	WbServer wbs = (WbServer) Naming.lookup(arglist[0]);
	if(wbs.newserver(arglist[1])) {
	    System.out.println("New Server on " + arglist[1] + " created sucessfully!"); 
	} else {
	    System.out.println("Failed to create server "+arglist[1]+" on server "+arglist[0]); 
	}
    }

    private void userInteract() {
	while (true) {
	    String choice = Invoke.promptAndGet(menu);
	    switch (choice.charAt(0)) {
	    case 's': serverCreate(); break; // create new server on another machine.
	    case 'a': addClientReq(); break; // add client to the server. 
	    case 'q': queryReq(); break;     // query board information on the server. 
	    case 't': transferReq(); break;  // transfer named board to new server.
	    case 'x': System.exit(0); break; // exit the program. 
	    }
	}
    }
	
    public static void main(String[] args) {
	try { WbAdmin wa = new WbAdmin(args); }
	catch(Exception e) {e.printStackTrace();}
    }
}

// -eof-
