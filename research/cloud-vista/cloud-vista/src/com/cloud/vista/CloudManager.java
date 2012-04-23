package com.cloud.vista;

import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.net.URLEncoder;
import java.io.BufferedReader;
import java.io.InputStreamReader;


/*
 * cloud manager to talk to cloud web server. 
 * functions include: 
 * 1, download visual frame files (maybe zipped); 
 * 2, check the progress of the hadoop jobs.
 * 3, check the status of the hadoop map/red jobs.  
 */
class CloudManager {
    private String serverBase = "http://knoesis157.cs.wright.edu:8080/cloudmanager.jsp"; 
    
    /**
     * Get the base url of the webserver.
     * @return
     */
    public String getServerBaseURL() {
		return serverBase;
    }
    
    /**
     * Set the base url of the cloud server.
     * @param baseurl
     * @return
     */
    public boolean setServerBaseURL(String baseurl) {
    	serverBase = baseurl;
    	return true;
    }
    
    /** thread class to start a new thread for the background progress check.
     *  
     * @author simon
     *
     */
    class progressChecker extends Thread {
    	public void run() {
    		MapRedProgressChecker.periodicCheck(); 
    	}
    }
   
    /**
     * Start up the background progress checking thread for the job.
     */
    public void updateProgress() {
    	Thread backthread = new progressChecker(); 
    	backthread.start();
    }
  
    /** 
     * brief request status of the hadoop job, e.g. RUNNING, TERMINATED etc.
     * @param req is the request number, 0 for request of status and 1 for request of progress.
     * @return String representing the job status, or progress. 
     */ 
    @Deprecated
    public String getJobStatus(int req) {
    	String reqStr = serverBase + "?requesttype=" + Integer.toString(req);
    	return cloudReader(reqStr); 
    }
 
    /** brief run hadoop job. 
     * @param hstr is the string to run the hadoop job. 
     * @return string of the job request result. 
     */ 
    @Deprecated
    public String runHadoopJob(String hstr) {
		// the hadoop request might contain non-url characters such as space, 
		// so, we need to encode it before doing the request. 
    	try {
    		String reqStr = serverBase + "?requesttype=2&hadoopstr=" + URLEncoder.encode(hstr, "UTF-8");
    		return cloudReader(reqStr);
    	} catch (Exception e) {
    		System.out.println("Encoding error. ");
    		return "ERROR";
    	}
    }
    
    /**
	 * Submitting a hadoop job. Implementation of this function under windows machine requires the plink.exe software
	 * which is used to remotely connect to the linux server and run the given hadoop command. Also, if you need to display
	 * the job running status and/or outputs, we need to get the process object and read from its' output stream. 
	 * @param none. 
	 * @return none.
	 */
	public void submitHadoopJob() {
		// for windows and linux OSes, we need to do different operations.
		String jobFileName = "cmd.sh.1"; // for windows machine only.
		String cloudServer = " nimbus.cs.wright.edu"; 
		String cmdStr = "../resources/plink.exe -ssh -l zhen -pw zhenli -m " + jobFileName + cloudServer;
		
		String resolution = VistaExplorer.Text_Resolution.getText();
		String numFrames = VistaExplorer.Text_NumberFrame.getText();
		String stepLength = VistaExplorer.Text_StepLength.getText();
		String maxSample = VistaExplorer.Text_MaxSample.getText();
		String sampleRate = VistaExplorer.Text_SampleRate.getText();
		String dataset = VistaExplorer.option.getText();
		String exploreSet = VistaExplorer.Text_Exploration.getText();
		
		String hadoopRunStr1 = "hadoop fs -rmr census_agg1 \n";
		String hadoopRunStr2 = "hadoop jar ./RR.jar -m 100 -r 20 -d /user/kekechen/census_norm5 -o census_agg1 -i 68 -n 20 -x 500 -y 500 -c 4 -l 0.1 -u 0 -v 0 -s 1 2>&1";
		
		try {
			String osName = System.getProperty("os.name").toLowerCase();
			System.out.println("You are running system on " + osName);
			
			if (osName.indexOf("linux") >= 0) {
				// this is linux operating system, then do.
				Runtime.getRuntime().exec(hadoopRunStr1); // delete the output dir first.
				cmdStr = "ssh -l ada " + cloudServer + " -C \"" + hadoopRunStr2 + "\""; 
				
			} else {
				// this is windows os, then do..
				FileWriter fwriter = new FileWriter(jobFileName);
				BufferedWriter out = new BufferedWriter(fwriter);
				out.write(". .bash_profile\n");
				out.write(hadoopRunStr1);
				out.write(hadoopRunStr2);
				out.close();
			}	
			// run the job and get local process for displaying output.
			Process p = Runtime.getRuntime().exec(cmdStr); 
	        
			// get output from process.
	        InputStream cmdOut = p.getInputStream(); 
	        InputStreamReader reader = new InputStreamReader(cmdOut); 
	        BufferedReader input = new BufferedReader(reader); 
	        String line; 
	        while((line = input.readLine()) != null) {
	            System.out.println(line); 
	        }
	        p.waitFor();
		} catch (Exception e) {
			System.out.println("Error while running hadoop job.");
		}
	}
	
	/**
	 * Download Visual files from the hadoop cloud. 
	 */
	public void getVisualFiles() {
		// get files from cloud server. 
		// if (stat == "DONE") {
		System.out.println("Downloading visual frame jobs......."); 
		if (true) {
			try {
				getFiles(10);
			} catch (Exception e) {
				System.err.println("Error Downloading files form cloud manager.");
			}
		}
		return; 
	}
  
    /** brief get files from the cloud server.
     * totalFrames is the total number of frames for the file.  
     * @param totalFrames
     * Total frame files to transfer. 
     * @return integer. 
     */ 
    private int getFiles(int totalFrames) throws IOException {
		for(int idx = 1; idx <= totalFrames; idx++) {
			String reqstr = serverBase + "?requesttype=3&fidx=" + Integer.toString(idx);
			String savedFileName = "part-0000" + Integer.toString(idx);
			
			// for reading data from the cloud server.
			java.io.BufferedInputStream in = new java.io.BufferedInputStream(new java.net.URL(reqstr).openStream());
			java.io.FileOutputStream fos = new java.io.FileOutputStream(savedFileName);
			java.io.BufferedOutputStream bout = new BufferedOutputStream(fos,1024);
			
			byte data[] = new byte[1024];
			System.out.print("Transfering file " + savedFileName + "."); 
			
			while(in.read(data,0,1024)>=0) {
				bout.write(data);
				System.out.print(".");
			}
			System.out.println("Done"); 
			bout.close();
			in.close();
		}
		return 0; 
    }
  
    /** brief request to the cloud and get response message. 
     * @param httpReqUrl the http request url. 
     * @return String the http request response message from cloud.
     */
    @SuppressWarnings("finally")
	private String cloudReader(String httpReqUrl) {
		String stat = "UNKNOWN";
		HttpURLConnection con = null; 
		URL address = null; 
		try {
		    address = new URL(httpReqUrl);
		    con = (HttpURLConnection)address.openConnection();
		    InputStream instrm = con.getInputStream(); 
		    int ch;
		    StringBuffer ss = new StringBuffer(); 
		    while((ch = instrm.read()) != -1)
			ss.append((char)ch);  
		    instrm.close(); 
		    stat = ss.toString();
		} catch (MalformedURLException e) {
		    e.printStackTrace();
		} catch (ProtocolException e) {
		    e.printStackTrace();
		} catch (IOException e) {
		    e.printStackTrace();
		} finally {
		    con.disconnect();
		    con = null;
		    return stat.trim();
		}
    }
}