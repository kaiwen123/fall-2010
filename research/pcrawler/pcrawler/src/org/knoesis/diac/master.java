package org.knoesis.diac;

import java.io.*;
import java.lang.*;
import java.util.ArrayList;
import java.util.*;
import java.util.concurrent.*;

/**
 * <H1>About</H1>
 * This is the master node for the crawler, it is responsible for 
 * managing the crawled user/link through a shared resource, such 
 * as the database (MySQL in my implementation). 
 * Also, this master will be responsible for distributing works/jobs
 * to the slave nodes upon request. 
 * <H1>How it works</H1>
 * The crawler will main two tables in the mySQL database.
 * One table called crawled table, which is used to store all the user ids
 * that has been crawled. 
 * 
 * The second table named tocrawle table, which stores all the user ids 
 * that has been distributed to the slave threads, due to its small size, 
 * this table can be maintained in the memory as a list or so. 
 * 
 * Also, there should be a in memory table that contains the raw user ids, raw here 
 * means we are not sure if these user ids has been crawled or not. So, there
 * need to be another process to check this. If the user id is verified to 
 * have not been crawled, then put it into the tobecrawled table in the DB. 
 * So, 
 * @author Simon Guo.
 *
 */
public class master {
	private String baseURL; 
	private String job; 

	// database info. 
	private String dbHostName = "";
	private static boolean connected = false; 
	
	// database tables;
	private String dbCrawledTable = "";
	private String dbToBeCrawledTable = "";
	
	// the list will maintain the currently being crawled ids. 
	private ArrayList<String> CrawlingIDs = new ArrayList<String>();
	
	// multi-threaded execution.
	private int poolSize = 2; 
	private int maxPoolSize = 5;
	
	private int keepAliveTime = 10;
	ThreadPoolExecutor threadPool = null;
	final ArrayBlockingQueue<Runnable> queue = new ArrayBlockingQueue<Runnable>(5);
	
	//TODO setup database tables. 
	//TODO Finish the next level of code. 
	//TODO make it distributed. 
	/**
	 *  Ids that has been successfully crawled by have not been
	 *  updated in the Database tables (remove from tobecrawled table and insert
	 *  into the c 
	 */
	private ArrayList<String> CrawledIDs = new ArrayList<String>();
	
	/**
	 * The main function.
	 * @param args
	 */
	public static void main (String args[]) {
		
	}
	
	/**
	 * Constructror.
	 */
	public master(){
		threadPool = new ThreadPoolExecutor(poolSize, maxPoolSize, 
						keepAliveTime, TimeUnit.SECONDS, queue);
	}
	/**
	 * Connect to the database. 
	 * @param dbURI
	 * @return
	 */
	private boolean dbConnect(String dbURI) {
		return true;
	}
	
	/**
	 * Get the base url of the crawl.
	 * @return
	 */
	public String getBaseURL() {
		return baseURL;
	}
	
	/**
	 * Request the user ids to crawl. 
	 * @return Arraylist of user ids. 
	 */
	public ArrayList<String> getJobUIDs() {
		ArrayList<String> uids = new ArrayList<String>(); 
		
		return uids;
	}
	
	/**
	 * This function is used to store the crawled ids into the database table. 
	 * @param ids
	 */
	public void setCrawledBatch(ArrayList<String> ids) {
	}
	
	/**
	 * Set a group of ids to be crawled. 
	 * @param ids
	 */
	public void setToBeCrawledBatch (ArrayList<String> ids) {
		
	}
	
	/**
	 * Determine if a given user id has been crawled or not.
	 * This will check with the database to see if the uid is already 
	 * in the table.
	 * @param uid
	 * @return true / false.
	 */
	private boolean isCrawled(String uid) {
		return true;
	}
	
	/**
	 * Set the uid to be crawled.
	 * Actually to move the uid from tobe crawled table into the crawled table. 
	 * @param uid
	 */
	private void setCrawled(String uid) {
		
	}
	
	/**
	 * Set a given user id to have been crawled. 
	 * @param uid
	 */
	private void setToBeCrawled(String uid) {
		
	}

	/**
	 * get a number of ids to crawl from the tobe crawled table.
	 * @param cnt How many ids do you want?
	 * @return A list of uids to crawl.
	 */
	private ArrayList<String> getIDsToCrawl(int cnt){
		ArrayList<String> uids = new ArrayList<String>();
		return uids;
	}
	
	/**
	 * Do the crawling using multiple threads.
	 * @param numThreads How many threads do you want to use?
	 * @return boolean
	 */
	public void multiThreadCrawling (int numThreads) {
		int i; 
		// first, let's create a pool of threads, this is more efficient.
		
		// then for each thread, let's give it the crawling job.
		while(true) {
			// get uids to be crawled. 
			ArrayList<String> ids = getIDsToCrawl(numThreads);
			Iterator<String> iter = ids.iterator();
			while(iter.hasNext()) {
				updateID(iter.next());
				threadPool.execute(new Runnable() {
					public void run() {
						doCrawling(getID());// do crawling.
					}
				});
			}
		}
	}
	
	// Runnable implementation can not access the id variable, this is a trick to 
	// work around this problem.
	private volatile String uid = "";
	
	public void updateID(String id){
		uid = id;
	}
	public String getID(){		
		return uid;
	}
	/**
	 * Do the crawling, by sending http request to the given url. 
	 * And then the crawled web page will be analyzed to get all the links/friends
	 * in the page, and these links/friend ids will be first checked to see if they
	 * have been crawled, if yes, then drop them, else store them into the tobe crawled
	 * database table. 
	 * 
	 * @param uid
	 * @return
	 */
	public boolean doCrawling(String uid) {
		String page = null;
		// get the url.
		
		// do the crawling by http request. 
		
		// analyze the page and obtain all the links, friends in the page. 
		
		// for each link/friend, check if they have already been crawled. 
		
		// if has been crawled, then remove from tobe crawled table. 
		// and insert into the already crawled table.
		
		// save the page into file. 
		try {
			FileWriter fwriter = new FileWriter("friends_page.txt");
			BufferedWriter out = new BufferedWriter(fwriter);
			out.write(page);
			out.close();
		} catch (Exception e) {
			System.out.println("Error while writing to file.");
		}
		
		
		// set the current uid as crawled. 
		setCrawled(uid);
		
		return true;
	}
	
	/**
	 * Get all the friends from the friend list.
	 * @param page the friend list page.
	 * @return a list of uids representing friends. 
	 */
	private ArrayList<String> getFriends(String page) {
		ArrayList<String> friends = new ArrayList<String>(); 
		
		return friends; 
	}
}
