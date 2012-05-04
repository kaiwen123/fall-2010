package org.knoesis.diac;

import java.io.*;
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
public class the_crawler {
	
	// the list will maintain the currently being crawled ids. 
	private ArrayList<String> CrawlingIDs = new ArrayList<String>();
	
	// multi-threaded execution.
	private int poolSize = 2; 
	private int maxPoolSize = 5;
	
	private int keepAliveTime = 10;
	ThreadPoolExecutor threadPool = null;
	final ArrayBlockingQueue<Runnable> queue = new ArrayBlockingQueue<Runnable>(5);
	
	// the social link manager. 
	private static link_manager lmanager = new link_manager(); 
	
	/**
	 * The main function.
	 * @param args
	 */
	public static void main (String args[]) {
		the_crawler m = new the_crawler();
		lmanager.importGraphFromFile("social_graph.txt", "tocrawl");
		
		// do the multi-thread crawling. 
		m.multiThreadCrawling(10); 
	}
	
	/**
	 * Constructror.
	 */
	public the_crawler(){
		// create thread pool. 
		threadPool = new ThreadPoolExecutor(poolSize, maxPoolSize, 
						keepAliveTime, TimeUnit.SECONDS, queue);
	}

	
	// Runnable implementation can not access the id variable, this is a trick to 
	// work around this problem.
	private volatile social_link tmp_link = null;
	
	public void updateID(social_link link){
		this.tmp_link = link;
	}
	public social_link getSocialLink(){		
		return this.tmp_link;
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
			ArrayList<social_link> links = lmanager.getIDsToCrawl(numThreads);
			Iterator<social_link> iter = links.iterator();
			while(iter.hasNext()) {
				updateID(iter.next());
				threadPool.execute(new Runnable() {
					public void run() {
						doCrawling(getSocialLink()); // do crawling.
					}
				});
			}
		}
	}

	/**
	 * Do the crawling, by sending http request to the given url. 
	 * And then the crawled web page will be analyzed to get all the links/friends
	 * in the page, and these links/friend ids will be first checked to see if they
	 * have been crawled, if yes, then drop them, else store them into the tobe crawled
	 * database table. 
	 * 
	 * @param link
	 * @return
	 */
	public boolean doCrawling(social_link link) {
		String url, page = null;
		// get the url.
		url = lmanager.getFriendsPageURL(link.getUid()); 
		
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
		return true;
	}
}
