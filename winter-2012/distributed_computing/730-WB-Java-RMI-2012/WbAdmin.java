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
    }

    private void serverCreate() {
	String args = Invoke.promptAndGet("ServerMachineName");
	Invoke.javaVM('S',  args);
    }

    private void addClientReq() {
	String args = Invoke.promptAndGet("BoardName DisplayOn ServerURL");
	Invoke.javaVM('C', args);
    }

    private void transferReq() {
	// Transfer a white board to a new server. For you TODO
    }

    private void queryReq() {     
	// Query for inforamtion from each server. For you TODO
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
