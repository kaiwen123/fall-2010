package com.cloud.vista;

import guicomponents.GButton;
import guicomponents.GWSlider;
import guicomponents.GCombo;
import guicomponents.GLabel;
import guicomponents.GSlider;
import guicomponents.GTextField;
import guicomponents.GVertSlider;
import guicomponents.GWindow;

import java.io.*;
import java.nio.file.FileSystem;
// import java.util.Vector;
import java.awt.Color; 
import java.awt.event.MouseEvent;
//import java.applet.*;

// import com.cloud.vista.cloudManager.Button;
// import com.cloud.vista.cloudManager.ImageButtons;

import processing.core.*;
import java.awt.event.*;
// import processing.app.*;

/**
 * The cloud vista explore, client program for exploring the vista system. 
 * @author Simon Guo. 
 *
 */
public class VistaExplorer extends PApplet {
	public VistaExplorer() {
		super();
	}
	
	private static final long serialVersionUID = 1L;

	/** 
	 * Widgets for Exploration control including buttons and slider bars. 
	 */
	private GButton m_btnZoomIn, m_btnZoomOut, m_btnPan, m_btnPlayFrame, m_btnPlayForward, m_btnPlayBackward;
	private GWSlider m_sbVisualFrame;
	private GVertSlider m_sliderZoom;
	GButton m_btnExploreManager;// The button to activate the exploration management window.
	static ExploreManager m_exploreManager = new ExploreManager(); // the exploration management window.
	
	// map and reduce progress bar.
	static GWSlider m_mapProgressbar, m_reduceProgressbar;
	
	/**
	 * Variables related to the visual parameters. 
	 */	
	private final float TOTAL_SCALES = 3.0f;
//	static final int m_totalFrameCount = 10; // number of visual frames in one exploration. 
	private String[] m_frameIds = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "10"};
	private float translateDelta = 300;
	private float m_translateLength = 30;
	Exploration m_currentExplore; 
	private VisualFrame m_currentVisualFrame; 
	
	public static int m_resolution;
	public static int m_viewX, m_viewY;
	private float m_scaleFactor;

	/**
	 * Visualization parameters. 
	 */
	static float m_translateX, m_translateY;

//	String[] lines, paramx, paramy;

	// directory for holding resources such as figures. 
	// data directory for holding visual frame data.
	static String m_resourceDir = "../resources/";
//	static String dataDir = m_resourceDir + "census_s1_10_1"; 

	static int m_currentViewID;

//	int startx, starty, endx, endy;
	static boolean stop = true;
	
	public static CloudManager m_cloudManager = new CloudManager(); 
	
	/**
	 * To configure the user interface, including setting up the locations of each widget, ticks
	 * on the slider bar and put labels on the widgets etc.  
	 */
	private void configUserInterface() {
		// the zooming buttons and sliders. 
	    m_sliderZoom = new GVertSlider(this, 40, 90, 10, 210);
	    m_sliderZoom.setValue(m_sliderZoom.getMinValue());       
	   
	    m_btnZoomIn = new GButton(this, m_resourceDir + "zoomIn.jpg", 1, 40, 80, 10, 10);
	    m_btnZoomOut = new GButton(this, m_resourceDir + "zoomOut.jpg", 1, 38, 303, 15, 15);
	    m_btnPan = new GButton(this, m_resourceDir + "RLUD.jpg", 1, 20, 20, 50, 50);
	    m_btnPlayFrame = new GButton(this, m_resourceDir + "play.jpg", 1, 290, 630, 15, 15);
	    m_btnPlayForward = new GButton(this, m_resourceDir + "forward.jpg", 1, 340, 630, 15, 15);
	    m_btnPlayBackward = new GButton(this, m_resourceDir + "backward.jpg", 1, 240, 630, 15, 15); 
	    
	    m_btnExploreManager = new GButton(this, "Manage Exploration", 80, 20, 100, 30);
	    
	    // show and set the progress bar of the mapper and reducer. 
	    @SuppressWarnings("unused")
		GLabel MapProgress=new GLabel(this, "Map", 200, 15, 100);
	    @SuppressWarnings("unused")
	    GLabel Reduce=new GLabel(this, "Reduce", 200, 55, 100);  

	    // progress bar of mapper and reducer.
	    m_mapProgressbar = new GWSlider(this, 270, 20, 300);
	    m_reduceProgressbar = new GWSlider(this, 270, 55, 300);
	    m_mapProgressbar.setPrecision(5);
	    m_reduceProgressbar.setPrecision(5);
	    m_mapProgressbar.setEnabled(false); m_mapProgressbar.setValue(0);
	    m_reduceProgressbar.setEnabled(false); m_reduceProgressbar.setValue(0);
	    
	    
		// slider bar to show play the visual frames. 
	    m_sbVisualFrame = new GWSlider(this, 50, 580, 500); 
	    m_sbVisualFrame.setTickLabels(m_frameIds);   
	    //m_sbVisualFrame.setRenderMaxMinLabel(false);
	    m_sbVisualFrame.setRenderValueLabel(false);
	    m_sbVisualFrame.setValue(0);
	    m_sbVisualFrame.setStickToTicks(true);
	}
	
	/**
	 * The initial display of the visual frames, which are displayed as heat maps. 
	 */
	private void initiateExploration(String exploreName) {
		// the exploration management window should be invisible now.
	    m_exploreManager.setVisible(true);
	    m_currentExplore = m_exploreManager.buildExploration(exploreName);
	    m_exploreManager.setVisible(false);
	}
	
	/**
	 * Setup the explore environment. This is 
	 * @param none.
	 * @return void.
	 */
	public void setup() {
		// set up the display convas. 
		this.setName("Cloud Vista Visual Explorer");
		
		size(600, 680, P2D);
	    m_translateX = 0;
	    m_translateY = -100;
	    
	    // configure the user interface. 
	    configUserInterface();
	    
	    // configure the visual frame display. 
	    initiateExploration("census_s1_10_1");
	    m_scaleFactor = 1;
	    smooth();
	    noStroke();
	    m_viewX = m_viewY = 0;
	    
	    m_resolution = 1;
	    frameRate(2);
	    stop = true; 
	    
	    frame.addMouseWheelListener(new MouseWheelInput());
	}
	
	// class for handling mouse wheel event. 
	class MouseWheelInput implements MouseWheelListener{  
		private int m_pointerX, m_pointerY;
		public void mouseWheelMoved(MouseWheelEvent e) {
			m_pointerX = e.getX();
			m_pointerY = e.getY();
			int step = e.getWheelRotation();  
			m_pointerX = m_pointerX + step * 5;  
			m_pointerY = m_pointerY + step * 5; 
		}  
	}
	
	/**
	 * Start the progress checking.
	 */
//	private void updateProgress() {
//		m_cloudManager.updateProgress();
//	}
	
	/**
	 * Handle Slider event for visual exploration.
	 * @param slider
	 */
	public void handleSliderEvents(GSlider slider) {
	    int step_length;
	    if(slider == m_sliderZoom) {
		    //System.out.println("Max: " + m_sliderZoom.getMaxValue()+", Min: "+m_sliderZoom.getMinValue()+", Value: "+m_sliderZoom.getValue());
		    m_scaleFactor = (float) (1.0 + 1.0 * TOTAL_SCALES * m_sliderZoom.getValue() / (m_sliderZoom.getMinValue() - m_sliderZoom.getMaxValue()));
		    m_translateX = -(m_scaleFactor - 1) * translateDelta;
		    m_translateY = -150 - (m_scaleFactor - 1) * translateDelta;
		    //System.out.println(m_scaleFactor);
		} else if(slider == m_sbVisualFrame) {
		    step_length = (m_sbVisualFrame.getMaxValue() - m_sbVisualFrame.getMinValue()) / 10;
		    System.out.println("visual step length: " + step_length);
		    m_currentViewID = m_sbVisualFrame.getValue() / step_length;
		}
	}

	/**
	 * events handler for buttons. 
	 * @param button the button object from the client.
	 */
	public void handleButtonEvents(GButton button) {
	    float step_length = (m_sliderZoom.getMinValue() - m_sliderZoom.getMaxValue()) / 10;
	    // System.out.println("step_length: " + step_length);
	    
	    if (button == m_btnZoomIn) {    
		    //System.out.println("Value(): " + m_sliderZoom.getValue());    
		    float now = m_sliderZoom.getValue() - step_length + m_sliderZoom.getMinValue();  
		    //System.out.println("now: " + now);
		    if(now > m_sliderZoom.getMinValue()) {
			    now = m_sliderZoom.getMinValue();
			}
		    //System.out.println("now: "+now);  
		    m_sliderZoom.setValue(now);
		    //System.out.println("Value: " + m_sliderZoom.getValue());
		    m_scaleFactor = m_scaleFactor + TOTAL_SCALES / 10;
		    
		    if(m_scaleFactor > 1.0 + TOTAL_SCALES) {
			    m_scaleFactor = (float) (1.0 + TOTAL_SCALES);
			}
		    
		    System.out.println("m_scaleFactor: " + m_scaleFactor);
		    m_translateX = -(m_scaleFactor - 1) * translateDelta;
		    m_translateY = -150 - (m_scaleFactor - 1) * translateDelta;
		} else if (button == m_btnZoomOut) {
		    float now = m_sliderZoom.getValue() - step_length;
		    if(now < m_sliderZoom.getMaxValue()) {
			    now = m_sliderZoom.getMaxValue();
			}
		    m_sliderZoom.setValue(now);
//		    System.out.println("now: "+now);
		    m_scaleFactor = m_scaleFactor - TOTAL_SCALES/10;
		    if(m_scaleFactor < 1.0) {
			    m_scaleFactor = 1.0f;
			}
//		    System.out.println("m_scaleFactor: "+m_scaleFactor);
		    m_translateX = -(m_scaleFactor - 1) * translateDelta;
		    m_translateY = -150 - (m_scaleFactor - 1) * translateDelta;
		} else if(button == m_btnPlayFrame) {
		    if(stop) {
		    	stop = false;
			    m_btnPlayFrame.setImages(m_resourceDir + "pause.jpg", 1);
			    
			    frameRate(1);
			} else {
				stop = true;
			    m_btnPlayFrame.setImages(m_resourceDir + "play.jpg", 1);
			    
			    frameRate(1);
			}
		} else if(button == m_btnPlayForward) {    
		    forward();
		} else if(button == m_btnPlayBackward) {
		    backward();
		} else if(button == m_btnPan) {
			// pan left, right, up and down according to the button.
//		    System.out.println("X: " + mouseX + ", Y: " + mouseY);
		    if(mouseX >= 30 && mouseX <= 60 && mouseY >= 20 && mouseY <= 40) {
		    	m_translateY += m_translateLength; // UP.
		    } else if(mouseX >= 30 && mouseX <= 60 && mouseY >= 50 && mouseY <= 70) {
			    m_translateY -= m_translateLength; // DOWN
			} else if(mouseX >= 20 && mouseX <= 38 && mouseY >= 30 && mouseY <= 60) {
			    m_translateX += m_translateLength; // LEFT
			} else if(mouseX >= 50 && mouseX <= 70 && mouseY >= 30 && mouseY <= 60) {
			    // RIGHT
			    m_translateX -= m_translateLength;
			}
		} else if(button == m_btnExploreManager) {
			m_exploreManager.setVisible(true);
	  }
	}

	/**
	 * next frame.
	 */
	public void forward() {
	    m_currentViewID += 1;
	    if (m_currentViewID >= m_currentExplore.getNumFrames()) {
		    m_currentViewID = 0;
		}
	    // System.out.println("m_currentViewID: "+m_currentViewID);
	    m_sbVisualFrame.setValueToTickNumber(m_currentViewID);
	    m_currentVisualFrame = m_currentExplore.getVisualFrame(m_currentViewID);
	}

	/**
	 * go to the previous frame.
	 */
	public void backward() {
	    m_currentViewID -= 1;
	    if (m_currentViewID < 0) {
		    m_currentViewID = m_currentExplore.getNumFrames() - 1;
		}
	    //System.out.println("m_currentViewID: "+m_currentViewID);
	    m_sbVisualFrame.setValueToTickNumber(m_currentViewID);
	    m_currentVisualFrame = m_currentExplore.getVisualFrame(m_currentViewID);
	}
	
	public void draw() { 
		background(176, 196, 222);
		if (stop == false) 
		    forward();   
		pushMatrix();
		translate(m_translateX, m_translateY);    
		scale(m_scaleFactor);
		m_currentVisualFrame = m_currentExplore.getVisualFrame(m_currentViewID);
		
		// draw for the current visual frame. 
		for (int i = 0; i < m_currentVisualFrame.m_visualFramePoints.size(); i++) {
			//System.out.println(m_visualFramePoints.size());
			Point p =(Point) m_currentVisualFrame.m_visualFramePoints.elementAt(i); 
			fill(p.m_pointColor.getRed(), p.m_pointColor.getGreen(), p.m_pointColor.getBlue());
			rect(p.getPointX() * m_resolution + m_viewX, p.getPointY() * m_resolution + m_viewY, m_resolution, m_resolution);
		} // end for.
		
		popMatrix(); 
		
		// rectangle selection. 
		if(mousePressed) {
			stroke(255,0,0);
			strokeWeight(5);
//			System.out.println("moused pressed.....");
			noFill();
			rect(m_selectionStartX, m_selectionStartY, mouseX-m_selectionStartX, mouseY-m_selectionStartY);
			System.out.println("draw rectangle.....");
			noStroke();
		}
	} // end draw.
	
	private float m_selectionStartX; 
	private float m_selectionStartY;
	private float m_selectionEndX; 
	private float m_selectionEndY; 
	/**
	 * When mouse pressed, then selection a the left top coordinate of the rectangle.
	 */
	// used for selection of areas.
	public void mousePressed() {
		m_selectionStartX = mouseX;
		m_selectionStartY = mouseY;
	}
	
	//TODO
	public void mouseReleased() {
		m_selectionEndX = mouseX;
		m_selectionEndY = mouseY; 
			
		// compute the subset. 
		System.out.println("computing the subset with rectangle: " + "<" + m_selectionStartX + "," + m_selectionStartY + ">  "
				+ "<" + m_selectionEndX + "," + m_selectionEndY + ">"); 
		m_exploreManager.buildSubsetExploration("name-to-be-added");
	}
	
	// mouse dragging event handling. 
//	public void mouseDragged()
//	{
//	  println("pmouseX:" + pmouseX + ",pmouseY:" + pmouseY);
//	  if(pmouseY >= 50 && pmouseY <= 570)
//	  {
//	    float step_x = mouseX - pmouseX;
//	    float step_y = mouseY - pmouseY;
//	    m_translateX += step_x;
//	    m_translateY += step_y;
//	  }  
//	}
}