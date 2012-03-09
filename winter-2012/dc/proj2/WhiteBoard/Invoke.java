/** @file: InvokeSystem.java @author pmateti@wright.edu */

package WhiteBoard;

import java.io.*;
import java.rmi.*;

public class Invoke {
    public static int verbosity = 1;	 // controls myPrint()
    private static BufferedReader stdIn; // stdinput, buffered
    private static int pid = 0;	// pseudo process-id

    //! The following Srings isolate path related issues.

    private static String javaCmd = "java"; // Verify the path
    // On Linux, e.g.: "/usr/lib/jvm/java-6-sun-1.6.0.06/bin/java";
    // On Windows, e.g.: "e:\\jdk1.6.10\\bin\\java";

    private static final String rmiRegistryPfx = "//localhost/";
    private static final String serverClass = "WhiteBoard.WbServerImpl";
    private static final String clientClass = "WhiteBoard.WbClientImpl";
    private static final String linesClass = "WhiteBoard.LinesFrameImpl";
    private static final String classNames[]
	= {serverClass, clientClass,linesClass};

    public static void setJavaCmdPath(String s) {
	javaCmd = s;
    }

    // for use during development
    public static void myPrint(String who, String what) {
	if (verbosity > 0)
	    System.out.println(who + ": " + what);
    }

    // post: return the users input; make sure it is non-empty
    public static String promptAndGet(String msg) {
	if (stdIn == null)
	    stdIn = new BufferedReader(new InputStreamReader(System.in));

	String s = null;
	try {
	    System.out.print(msg + ": ");
	    System.out.flush();
	    s = stdIn.readLine();
	} catch (Exception e) {e.printStackTrace();}
	if (s == null || s == "") s = "empty";
	return s;
    }

    /** Invoke java on the given class in a separate process.
	Also, take care of any exceptions. */

    public static void javaVM(char c, String args) {
	pid ++;
	String classNm = classNames[c == 'S'? 0 : c == 'C' ? 1 : 2];
	String cmd = javaCmd + " " +  classNm + " " + " " +  args;
	System.out.println("In Invoke: " + cmd); 
	try {
	    Runtime r = Runtime.getRuntime();
	    myPrint("Runtime "  + r, cmd);
	    try {
		Process p = r.exec(cmd);
		myPrint("Process", "" + p);
	    } catch(Exception e) {
		System.out.println("Error executing r.exec(" + cmd + ")");
	    }
	} catch(Exception e) {
	    System.out.println("Runtime Error executing [" + cmd + "]");
	}
    }

    public static String makeURL(char c, String id) {
	return rmiRegistryPfx + c + id;
    }

    public static Remote lookup(String url) {
	for (int j = 0; j < 10; j++) 
	    try {
		return Naming.lookup(url);
	    } catch (Exception e) {
		myPrint("Naming.lookup(", url + ") not found yet ...");
		try {Thread.sleep(1000);}
		catch (Exception x)  {x.printStackTrace();}
	    }
	return null;
    }

    public static void main(String[] args) {
	Invoke.javaVM('S', "");
    }
}

// -eof-
