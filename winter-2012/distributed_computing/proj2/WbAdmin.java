// file: WbAdmin.java by pmateti@wright.edu
// This is a "shell" for the WhiteBoard project of CEG 730

package WhiteBoard;

import java.io.*;
import java.util.*;

public class WbAdmin {

    private Vector vServers;

    private static final String menu = "\nWbAdmin: create a " +
	"[s]erver, [a]dd client, [q]uery, [t]ransfer, e[x]it";

    public WbAdmin() {
	vServers = new Vector();
	String serverUrl; // TODO.
	vServers.addElement((WbServer) Invoke.lookup(serverUrl));
    }

    private void serverCreate() {
	String args = Invoke.promptAndGet("ServerMachineName");
	Invoke.javaVM('S',  args);
    }

    private void addClientReq() {
	String args = Invoke.promptAndGet("BoardName DisplayOn ServerURL");
	Invoke.javaVM('C', args);
    }

    // Transfer a white board to a new server. 
    private void transferReq() {
	String args = Invoke.promptAndGet("OldServer boardnm NewServer");
	try { vServers[0].transfer(argv[1], argv[2]); }
	catch (Exception e) { e.printStackTrace(); }
    }

    // Query for inforamtion from each server. 
    private void queryReq(String serverUrl) {
	WbServer wbserver = (WbServer) Naming.lookup(serverUrl); 
	vServer.addElement(wbserver);
	
	WbServer wbs = wbserver.query(); 
	int brdcnt = 0, clntcnt, lncnt; 
	for(Enumeration es = wbs.elements(); es.hasMoreElements(); ) {
	    System.out.println("Board: " + es.boardName); 
	    clntcnt = 0, lncnt = 0; 
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

    private void userInteract() {
	while (true) {
	    String choice = Invoke.promptAndGet(menu);
	    switch (choice.charAt(0)) {
	    case 's': serverCreate(); break;
	    case 'a': addClientReq(); break;
	    case 'q': queryReq(); break;
	    case 't': transferReq(); break;
	    case 'x': System.exit(0); break;
	    }
	}
    }
	
    public static void main(String[] args) {
	WbAdmin wa = new WbAdmin();
	wa.userInteract();
    }
}

// -eof-
