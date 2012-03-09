// file: WbAdmin.java by pmateti@wright.edu
// This is a "shell" for the WhiteBoard project of CEG 730

package WhiteBoard;

import java.io.*;
import java.util.*;
import java.rmi.*;

public class WbAdmin extends java.rmi.server.UnicastRemoteObject {
    private String serverURL, adminNm, myURL; 
    private WbServer myserver;
    private Vector vServers;	// store all the connected servers. 

    private static final String menu = "\nWbAdmin: create a " +
	"[s]erver, [a]dd client, [q]uery, [t]ransfer, e[x]it";

    // constructor. 
    public WbAdmin(String[] args) throws Exception {
	super(); 
    }

    /**
     * Request server to add a new client. 
     */
    private void addClientReq() {
	// args should be = [clientId, brdNm,displayMcnm,wbserverURL,color].
	String args = Invoke.promptAndGet("clientId brdNm DisplayOn ServerURL color");
	args = "java WhiteBoard.WbClientImpl " + args;
	try {Runtime.getRuntime().exec(args); }
	catch (Exception e) { e.printStackTrace(); }
    }

    /** Transfer a white board to a new server. 
     * @pre old server URL, board name to be transfered and new server URL was entered by user from
     * keyboard correctly. 
     * @post The named board boardnm was transfered from old server to new server. 
     */ 
    private void transferReq() {
	String argstr = Invoke.promptAndGet("OldServerURL boardnm NewServerURL");
	String[] args = argstr.split(" ");
	
	try {
	    WbServer wbs = (WbServer) Naming.lookup(args[0]); 
	    wbs.transfer(args[1], args[2]); 
	} catch (Exception e) { e.printStackTrace(); }
    }

    /** Query for inforamtion from each server. 
     * @pre a valid server url was enter by user from keyboard. 
     * @post board information on the server was returned and processed. 
     */
    private void queryReq() {
	String args = Invoke.promptAndGet("QueryServerURL");
	try { 
	    WbServer wbs = (WbServer) Naming.lookup(args); 

	    // query server and get object. 
	    Vector <ABoard> abs = wbs.query(); 
	    if (abs.size() == 0) { 
		System.out.println("No boards on server. ");
		return; 
	    }

	    int absnum = abs.size(); 
	    while (absnum > 0) {
		System.out.println(abs.get(absnum-1).boardName); 

		// clients. 
		Vector<WbClient> wbcs = abs.get(absnum-1).vClients;
		int cnum = wbcs.size(); 
		System.out.println("Total Clients: " + Integer.toString(cnum)); 
		while(cnum > 0) {
	    	    System.out.println("Machine: " + wbcs.get(cnum-1).getMcName() + 
				       " MYURL: " + wbcs.get(cnum-1).getmyURL());
		    cnum--; 
		}

		// lines. 
		Vector<LineCoords> wbls = abs.get(absnum-1).vLines;
		int lnum = wbls.size(); 
		System.out.println("Total Lines: " + Integer.toString(lnum)); 
		while(lnum > 0) {
		    System.out.println("(" + Integer.toString(wbls.get(lnum-1).x1) + 
				       ", " + Integer.toString(wbls.get(lnum-1).y1) +
				       ") --> (" + Integer.toString(wbls.get(lnum-1).x2) +
				       ")" + Integer.toString(wbls.get(lnum-1).y2)); 
		    lnum--; 
		}		
		absnum--; 
	    }
	} catch (Exception e) { e.printStackTrace(); }
    }

    /**
     * @biref Create new server, prompt user to enter id and location of new server, then use the runtime
     * call to generate a new server. 
     * @pre this func was called. 
     * @post new server was created or exception happens, stack trace print out.
     * @param none. 
     * @return void. 
     */
    private void serverCreate() {
	String args1 = Invoke.promptAndGet("newserverid serverlocation");
	Invoke.javaVM('S', args1); 
	//try { Runtime.getRuntime().exec("java WhiteBoard.WbServerImpl " + args1); }
	//catch (Exception e) {e.printStackTrace();}

	// String args = Invoke.promptAndGet("OldServerURL NewServerName NewServerLocation"); 
	// String [] arglist = args.split(" ");

	// try { 
	//     WbServer wbs = (WbServer) Naming.lookup(arglist[0]); 
	//     wbs.newserver(arglist[1], arglist[2]);
	// } catch(Exception e) { e.printStackTrace(); }
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
	
    // main func. 
    public static void main(String[] args) {
	try { 
	    WbAdmin wa = new WbAdmin(args);
	    wa.userInteract(); 
	}
	catch(Exception e) {e.printStackTrace();}
    }
}

// -eof-
