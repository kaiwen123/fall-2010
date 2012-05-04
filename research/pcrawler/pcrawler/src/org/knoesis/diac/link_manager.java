package org.knoesis.diac;

//for connection to mysql.
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.regex.Pattern;

/**
 * The purpose of this class is to manager the links for crawling and
 * to be crawled. 
 * @author Simon Guo. 
 * @version 1.0
 * 
 */
public class link_manager {
	private String baseURL = "http://www.facebook.com/"; 
	private String job; 

	// database info. 
	private String dbHostName = "";
	private static boolean connected = false; 
	
	// database tables;
	private String dbCrawledTable = "";
	private String dbToBeCrawledTable = "";
	
	// setup database tables. 
	// created tables users_tocrawl and users_crawled. 	
	private String table_tocrawl = "users_tocrawl";
	private String table_crawled = "users_crawled";
	
	// database connection. 
	private String dbConStr = "jdbc:mysql://localhost:3306/pcrawler?user=root&password=";
	private Connection conn = null;
	
	// database operations.
	private java.sql.Statement qstmt = null;
	private java.sql.PreparedStatement pstmt = null;
	private ResultSet rset = null;
	
	link_manager() {
		// get connection to the database. 
		try {
			conn = DriverManager.getConnection(dbConStr);
			conn.setAutoCommit(false);
		} catch (SQLException e) {
			System.err.println("Error connecting to mysql server.");
			e.printStackTrace();
		}
	}
	
	/**
	 * Get the base url of the crawl.
	 * @return
	 */
	public String getBaseURL() {
		return baseURL;
	}
	
	/**
	 * Get url of a user's friend page. And if the user has a unique uid, then 
	 * we will use the uid to construct the url, or else, we use other methods.
	 * @param fid the id of a friend. 
	 * @return String representing the user url of the fid's friends page. 
	 */
	public String getFriendsPageURL(String fid) {
		String url; 
		Pattern p = Pattern.compile("^[0-9]+$");
		if(p.matcher(fid).matches()){
			// only numbers.
			url = baseURL + "profile.php?id=" + fid + "&sk=friends";
		} else {
			// uids. 
			url = baseURL + fid + "?sk=friends";
		}
		return url;
	}
	
	/**
	 * Request the user ids to crawl. 
	 * @return Arraylist of user links. 
	 */
	public ArrayList<social_link> getJobLinks() {
		ArrayList<social_link> crawled_list = new ArrayList<social_link>(); 
		String sqlstr = "SELECT * FROM " + table_tocrawl;
		
		try {
			qstmt = conn.createStatement();
			rset = qstmt.executeQuery(sqlstr);
			while(rset.next()) {
				crawled_list.add(new social_link(rset.getString("userID"), rset.getString("fromID"), rset.getString("desc")));
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return crawled_list;
	}
		
	/**
	 * Connect to the database. 
	 * @return true on success and false on failure.
	 */
	private boolean connectTODB() {
		try {
			conn = DriverManager.getConnection(dbConStr);
			conn.setAutoCommit(false);
		} catch (SQLException e) {
			System.err.println("Error connecting to mysql server.");
			e.printStackTrace();
		}
		return true;
	}
	
	/**
	 * Set a group of ids to be crawled. 
	 * @param links a group of user social links to be inserted into tocrawl table. The format of id
	 * should not only contain the id itself, but also the from user id. 
	 * @return void.
	 */
	public void setToBeCrawledBatch (ArrayList<social_link> links) {
		try {
			String sqlstr = "INSERT INTO " + table_tocrawl + " VALUES (?, ?, ?)"; 
			int cnt = 0;
			
			Iterator<social_link> iter = links.iterator();
			while (iter.hasNext()) {
				social_link link = iter.next();
				pstmt = conn.prepareStatement(sqlstr);
				pstmt.setString(1, link.getUid());
				pstmt.setString(2, link.getFromID());
				pstmt.setString(3, link.getDesc());
				pstmt.addBatch();
				cnt++; 
				
				if (cnt % 100 != 0) pstmt.executeBatch(); 
			}
			pstmt.executeBatch();
			System.out.println("Added " + Integer.toString(cnt) + " to table " + table_tocrawl);
			
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Determine if a given user id has been crawled or not.
	 * This will check with the database to see if the uid is already 
	 * in the table.
	 * @param link The link to be checked.
	 * @return true / false.
	 */
	public boolean isCrawled(social_link link) {
		try {
			qstmt = conn.createStatement();
			rset = qstmt.executeQuery("SELECT * FROM " + table_crawled + " WHERE userID = " + link.getUid());
			if(rset.getFetchSize() > 0) return true; 
			else return false; 
		} catch (SQLException e) {
			e.printStackTrace();
		}
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
	
	/**
	 * Set the uid to be crawled.
	 * Actually to move the uid from tobe crawled table into the crawled table. 
	 * @param uid
	 */
	@Deprecated
	public void setCrawled(social_link link) {
		// use the batch function. 
	}
	
	/**
	 * Set a given user id to have been crawled. 
	 * @param uid
	 */
	@Deprecated
	public void setToBeCrawled(social_link link) {
		
	}

	/**
	 * get a number of ids to crawl from the tobe crawled table.
	 * @param cnt How many ids do you want?
	 * @return A list of social_link to crawl.
	 */
	public ArrayList<social_link> getIDsToCrawl(int cnt){
		ArrayList<social_link> links = new ArrayList<social_link>();
		String sqlstr = "";
		
		sqlstr = "SELECT * FROM " + table_tocrawl + " LIMIT " + Integer.toString(cnt);
		
		try {
			qstmt = conn.createStatement();
			rset = qstmt.executeQuery(sqlstr);
			while(rset.next()) {
				links.add(new social_link(rset.getString("userID"), rset.getString("fromID"), rset.getString("desc")));
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return links;
	}

	
	/**
	 * Get all the user ids from the database table. 
	 * @param type
	 * @return A list of user ids.
	 */
	public ArrayList<String> getAllUserID(String type) {
		ArrayList<String> crawled_list = new ArrayList<String>();
		String sqlstr = "";
		
		if (type == "crawled") sqlstr = "SELECT * FROM " + table_crawled;
		else if (type == "tocrawl") sqlstr = "SELECT * FROM " + table_tocrawl;
		else {
			System.out.println("Operation type error, aborted.");
			return null;
		}
		
		try {
			qstmt = conn.createStatement();
			rset = qstmt.executeQuery(sqlstr);
			while(rset.next()) {
				String uid = rset.getString("userID");
				crawled_list.add(uid);
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return crawled_list;
	}
	
	/**
	 * Import graph from a file. 
	 * @param fname The name of file to import from.
	 * @param type What type of graph is this? crawled or tocrawl? 
	 * @return true on success, and false otherwise.
	 */
	public boolean importGraphFromFile(String fname, String type) {
		String sqlstr = "INSERT INTO "; 
		if (type == "crawled") sqlstr += table_crawled;
		else if (type == "tocrawl") sqlstr += table_tocrawl;
		else {
			System.out.println("Operation type error, aborted.");
			return false;
		}
		
		sqlstr += " VALUES (?, ?, ?)";
		
		try {
			FileInputStream finput = new FileInputStream(fname);
			BufferedReader reader = new BufferedReader(new InputStreamReader(finput));
			String line; int ln = 0;
			
			while((line = reader.readLine()) != null) {
				// System.out.println(line);
				ln++;
				String parts[] = line.trim().split(" ", 3);
				// sqlstr += "(" + parts[0] + "," + parts[1] + "," + parts[2] + ")";
				pstmt = conn.prepareStatement(sqlstr);
				pstmt.setString(1, parts[0]);
				pstmt.setString(2, parts[1]);
				pstmt.setString(3, parts[2]);
				pstmt.addBatch();
				if (ln % 100 != 0) pstmt.executeBatch();
			}
			pstmt.executeBatch();
			System.out.println("Inserted " + Integer.toString(ln) + " Records.");
			conn.commit();
		} catch (SQLException e1) {
			e1.printStackTrace();
			return false;
		} catch (FileNotFoundException e1) {
			e1.printStackTrace();
			return false; 
		} catch (IOException e) {
			e.printStackTrace();
			return false; 
		}
		return true;
	}
	
	/**
	 * This function is used to store the crawled ids into the database table. 
	 * @param links the links that has been crawled. So, first we need to insert
	 * these links into the crawled table and then delete them from the tocrawl table.
	 * @return void.
	 */
	public void setCrawledBatch(ArrayList<social_link> links) {
		String sqlstr = "INSERT INTO " + table_tocrawl + " VALUES (?, ?, ?)";
		int cnt = 0; 
		
		try {
			// insert into the crawled table. 
			Iterator<social_link> iter = links.iterator();
			while (iter.hasNext()) {
				social_link link = iter.next();
				pstmt = conn.prepareStatement(sqlstr);
				pstmt.setString(1, link.getUid());
				pstmt.setString(2, link.getFromID());
				pstmt.setString(3, link.getDesc());
				pstmt.addBatch();
				cnt++; 
				
				if (cnt % 100 != 0) pstmt.executeBatch(); 
			}
			pstmt.executeBatch();
			System.out.println("Added " + Integer.toString(cnt) + " to table " + table_tocrawl);
			
			cnt = 0;
			// delete from the to crawl table. 
			sqlstr = "INSERT INTO " + table_crawled + " VALUES (?, ?, ?)";
			iter = links.iterator();
			while (iter.hasNext()) {
				social_link link = iter.next();
				pstmt = conn.prepareStatement(sqlstr);
				pstmt.setString(1, link.getUid());
				pstmt.setString(2, link.getFromID());
				pstmt.setString(3, link.getDesc());
				pstmt.addBatch();
				cnt++; 
				
				if (cnt % 100 != 0) pstmt.executeBatch(); 
			}
			pstmt.executeBatch();
			System.out.println("Added " + Integer.toString(cnt) + " to table " + table_crawled);	
		} catch (SQLException e) {
			e.printStackTrace();
		}
	}
}
