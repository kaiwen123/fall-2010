package com.cloud.vista;

import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Vector;

import processing.core.PApplet;
/**
 * The exploration object for the system. 
 * one exploration is composed of a number of visual frames, 
 * these frames are kept in a hash map with the visual frame id 
 * as the key of the map and the visual frame as the value. 
 * @author simon guo.
 *
 */
public class Exploration {
	private String m_exploreLocalFolder = "";
	private int m_numFrames = 0; 
	private HashMap<Integer, VisualFrame> m_visualFramesOfExploration = new HashMap<Integer, VisualFrame>(); 
	// exploration information. 
	private String exploreType; 
	private float scale; 
	private float step_length; 
	private int resolution; 
	private DataSet m_dataset;
	private int nsteps; // number of frames in this exploration. 
	
	// getters. 
	String getExploreType() { return exploreType; }
	float getScale() { return scale; }
	float getStepLength() { return step_length; }
	int getResolution() { return resolution; }
	String getDataSetName() { return m_dataset.getName(); }
	int getDataSetDim() { return m_dataset.getDim(); }
	int getNumSteps() { return nsteps; }
	
	// setters. 
	void setExploreType(String type) { exploreType = type; }
	void setScale(int s) { scale = s; }
	void setStepLength(float sl) { step_length = sl; }
	void setResolution(int r) { resolution = r; }
	void setDataset(String dname, int ddim) { m_dataset = new DataSet(dname, ddim); }
	void setNumSteps(int s) { nsteps = s; }
	
	/**
	 * The dataset class. 
	 * @author simon
	 *
	 */
	class DataSet {
		private String m_datasetName; 
		private int m_datasetDim; 
		private Vector<String> m_explorations = new Vector<String>(); 
		
		DataSet(String name, int dim) {
			m_datasetName = name; 
			m_datasetDim = dim; 
		}
		
		//getters and setters. 
		String getName() {return m_datasetName; }
		int getDim() { return m_datasetDim; }
		void setName(String dName) { m_datasetName = dName; }
		void setDim(int dim) { m_datasetDim = dim; }
		
		void addExploration(String expName) { m_explorations.add(expName); }
		void deleteExploration(String expName) { m_explorations.remove(expName); }
		Vector<String> getExplorations() { return m_explorations; }
	}		
	
	public Exploration() {}
	
	/**
	 * Build visual frames from the files in the given local
	 * exploration directory. 
	 * @return true on success and false on failure. 
	 */
	public boolean buildVisualFrames() {
		m_numFrames = 0; 
		File localDir = new File(m_exploreLocalFolder);
		File[] visualFiles = localDir.listFiles();
		for(int i = 0; i < visualFiles.length; i++) {
			if(visualFiles[i].isFile() && visualFiles[i].getName().startsWith("part-")) {
				VisualFrame vf = renderVisualFrame(visualFiles[i].getAbsolutePath());
				m_visualFramesOfExploration.put(vf.getVisualFrameID(), vf);
				m_numFrames++;
			}
		}
		 
		return true; 
	}
	
	/**
	 * delete all frames from this exploration, or regenerate the visual frames. 
	 * @return true on success and false otherwise. 
	 */
	public boolean purgeVisualFrames() {
		m_visualFramesOfExploration.clear();
		return true; 
	}
	
    /**
     * @param fileFullName the full name including the path of the file where visual frame is 
     * kept.
     * @param vid the id of the visual frame to render.
     * @return The visual frame that has been rendered.  
     */
    private VisualFrame renderVisualFrame(String fileFullName) {
    	// read data from visual frame file. 
    	String line;
  	  
	    float maxdense = 0;
	    BufferedReader reader = null;
	    try {
	    	FileInputStream fstream = new FileInputStream(fileFullName);
		    // Get the object of DataInputStream
		    DataInputStream in = new DataInputStream(fstream);
		    reader = new BufferedReader(new InputStreamReader(in));	
	    } catch (Exception e) {
	    	System.out.println("Error reading from file. ");
	    }
	    
	    VisualFrame v = new VisualFrame(0); 
	    float dense = 0;
	    String[] parts;
	    while(true) {
		    try {
			    line = reader.readLine();
			    if (line == null || line == "") break;
			    parts = line.trim().split("\t");
			} catch (IOException e) {
			    System.out.println("Error parsing visual frame files. ");
				break;
			}
		    
		    String[] p2 = PApplet.split(parts[0], '|');
		    
		    // visual frame id. 
		    int vid = Integer.parseInt(p2[0]);
		    v.setVisualFrameID(vid); 

		    Point p = new Point(p2[1], p2[2], parts[1]);
    
			if (p.m_pointDensity > maxdense)
			maxdense = p.m_pointDensity;
			
			dense += p.m_pointDensity;  
			v.addPoint(p);
		}
	    v.normalize(maxdense, dense);
    	return v; 
    }
	
	/**
	 * Obtain the local folder of the exploration. 
	 * @return
	 */
	public String getExploreLocalFolder() {
		return m_exploreLocalFolder;
	}
	
	/**
	 * set the local folder for the exploration. 
	 * @return true if file exists and otherwise false. 
	 */
	public boolean setExplorationLocalFolder(String localfolder) {
		File file = new File(localfolder);
		boolean exists = file.exists();
		if(exists) m_exploreLocalFolder = localfolder;
		return exists; 
	}
	
	/**
	 * Obtain number of frames for this exploration. 
	 * @return number of frames as integer.
	 */
	public int getNumFrames() {
		return m_numFrames; 
	}
	
	/**
	 * get a specified visual frame. 
	 * @param vid the id of desired visual frame. 
	 * @return a VisualFrame 
	 */
	public VisualFrame getVisualFrame(int vid) {
		return m_visualFramesOfExploration.get(vid);
	}
}
