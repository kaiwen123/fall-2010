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
// import java.util.Vector;
import java.awt.Color; 
import java.awt.event.MouseEvent;
//import java.applet.*;

// import com.cloud.vista.cloudManager.Button;
// import com.cloud.vista.cloudManager.ImageButtons;

import processing.core.*;
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

	// for calling of the processing public functions, e.g. fill, draw etc. 
	public static CloudManager manager = new CloudManager(); 

	static final float SCALE_TOT = 3;
	static final int maxframe = 10;
	static final String[] label = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "10"};
	static float translateDelta = 300;
	static float translateLength = 30;
	static View currentv; 
	public static int resol, viewx, viewy, radius;
	static float scale_factor;

	static float translateX, translateY;

	String[] lines, paramx, paramy;

	// directory for holding resources such as figures. 
	// data directory for holding visual frame data.
	static String resourceDir = "C:/data/";
	static String dataDir = resourceDir + "census_s1_10_1"; 

	
	private BufferedReader reader;
	static int cvid;

	int startx, starty, endx, endy;
	static boolean stop = true;

	// button for visual control and job control. 
	static GButton ZoomIn, ZoomOut, TranslateButton, PlayButton, ForwardButton, BackButton;
	static GWSlider scrollbar;
	static GVertSlider ZoomButton;
	// ImageButtons button;

	// load data button and the parameter window.
	GButton LoadData;
	GWindow Parameter;
	
	// the parameter setting and exploration management windows.
	static LoadManager loadMgr = new LoadManager(); 
	
	// map and reduce progress bar.
	static GWSlider MapProgressbar, ReduceProgressbar;

	// Below are plut-in for the window of loading data
	static GButton Load;
	
	static View[] views;
	/**
	 * Setup the explore environment.
	 * @param none.
	 * @return void.
	 */
	public void setup() {
		size(600, 680);
		//size(1000, 1000);
	    translateX = 0;
	    translateY = -100;
	    
	    this.setName("Cloud Vista Visual Explorer");
	    // the exploration management window should be invisible now.
	    loadMgr.setVisible(false);
	    
	    // for compatibility considerations.
		File fresDir = new File(resourceDir);
		
		if (!fresDir.exists()) {
			resourceDir = "../resources/";
			dataDir = resourceDir + "visual_frame_data/census_s1_10_1";
		}
	    
	    scrollbar = new GWSlider(this, 50, 580, 500); 
	    scrollbar.setTickLabels(label);   
	    //scrollbar.setRenderMaxMinLabel(false);
	    scrollbar.setRenderValueLabel(false);
	    scrollbar.setValue(0);
	    scrollbar.setStickToTicks(true);
	   
	    ZoomButton = new GVertSlider(this, 40, 90, 10, 210);
	    //System.out.println(ZoomButton.getMinValue());
	    ZoomButton.setValue(ZoomButton.getMinValue());       
	    //System.out.println(ZoomButton.getValue());
	   
	    ZoomIn = new GButton(this, resourceDir + "zoomIn.jpg", 1, 40, 80, 10, 10);
	    ZoomOut = new GButton(this, resourceDir + "zoomOut.jpg", 1, 38, 303, 15, 15);
	    TranslateButton = new GButton(this, resourceDir + "RLUD.jpg", 1, 20, 20, 50, 50);
	    PlayButton = new GButton(this, resourceDir + "play.jpg", 1, 290, 630, 15, 15);
	    ForwardButton = new GButton(this, resourceDir + "forward.jpg", 1, 340, 630, 15, 15);
	    BackButton = new GButton(this, resourceDir + "backward.jpg", 1, 240, 630, 15, 15); 
	    
	    LoadData = new GButton(this, "Load Data", 500, 110, 70, 15);
	    
	    @SuppressWarnings("unused")
		GLabel MapProgress=new GLabel(this, "Map", 300, 20, 100);
	    @SuppressWarnings("unused")
	    GLabel Reduce=new GLabel(this, "Reduce", 300, 50, 100);  

	    // progress bar of mapper and reducer.
	    MapProgressbar = new GWSlider(this, 370, 20, 200);
	    ReduceProgressbar = new GWSlider(this, 370, 50, 200);
	    MapProgressbar.setEnabled(false);
	    ReduceProgressbar.setEnabled(false);
	     
	    scale_factor = 1;
	    smooth();
	    noStroke();
	    radius = 10000;
	    viewx = viewy = 0;
	    
	    resol=1;
	    getParameter(dataDir + "_params");
	  
	    views = new View[maxframe];
	    convert(); 
	    frameRate(2);
	    stop = true;
	    
	    //updateProgress(); // update progress now.
	}
	
	/**
	 * Start the progress checking.
	 */
	private void updateProgress() {
		manager.updateProgress();
	}
	
	/**
	 * Handle Slider event for visual exploration.
	 * @param slider
	 */
	public void handleSliderEvents(GSlider slider) {
	    int step_length;
	    if(slider == ZoomButton) {
		    //System.out.println("Max: " + ZoomButton.getMaxValue()+", Min: "+ZoomButton.getMinValue()+", Value: "+ZoomButton.getValue());
		    scale_factor = (float) (1.0 + 1.0 * SCALE_TOT * ZoomButton.getValue() / (ZoomButton.getMinValue()-ZoomButton.getMaxValue()));
		    translateX = - (scale_factor - 1) * translateDelta;
		    translateY = -150 - (scale_factor - 1) * translateDelta;
		    //System.out.println(scale_factor);
		} else if(slider == scrollbar) {
		    step_length = (scrollbar.getMaxValue() - scrollbar.getMinValue()) / 10;
		    cvid = scrollbar.getValue() / step_length;
		}
	}

	/**
	 * events handler for buttons. 
	 * @param button the button object from the client.
	 */
	public void handleButtonEvents(GButton button) {
	    float step_length = (ZoomButton.getMinValue() - ZoomButton.getMaxValue()) / 10;
	    // System.out.println("step_length: " + step_length);
	    
	    if (button == ZoomIn) {    
		    //System.out.println("Value(): " + ZoomButton.getValue());    
		    float now = ZoomButton.getValue() - step_length + ZoomButton.getMinValue();  
		    //System.out.println("now: " + now);
		    if(now > ZoomButton.getMinValue()) {
			    now = ZoomButton.getMinValue();
			}
		    //System.out.println("now: "+now);  
		    ZoomButton.setValue(now);
		    //System.out.println("Value: " + ZoomButton.getValue());
		    scale_factor = scale_factor + SCALE_TOT / 10;
		    
		    if(scale_factor > 1.0 + SCALE_TOT) {
			    scale_factor = (float) (1.0 + SCALE_TOT);
			}
		    
		    System.out.println("scale_factor: " + scale_factor);
		    translateX = -(scale_factor - 1) * translateDelta;
		    translateY = -150 - (scale_factor - 1) * translateDelta;
		} else if (button == ZoomOut) {
		    float now = ZoomButton.getValue() - step_length;
		    if(now < ZoomButton.getMaxValue()) {
			    now = ZoomButton.getMaxValue();
			}
		    ZoomButton.setValue(now);
		    System.out.println("now: "+now);
		    scale_factor = scale_factor - SCALE_TOT/10;
		    if(scale_factor < 1.0) {
			    scale_factor = 1.0f;
			}
		    System.out.println("scale_factor: "+scale_factor);
		    translateX = -(scale_factor-1)*translateDelta;
		    translateY = -150 - (scale_factor-1)*translateDelta;
		} else if(button == PlayButton) {
		    if(stop == true) {
		    	stop = false;
			    PlayButton.setImages(resourceDir + "pause.jpg", 1);
			          
			    frameRate(1);
			} else {
				stop = true;
			    PlayButton.setImages(resourceDir + "play.jpg", 1);
			    
			    frameRate(1);
			}
		} else if(button == ForwardButton) {    
		    forward();
		} else if(button == BackButton) {
		    backward();
		} else if(button == TranslateButton) {
		    System.out.println("X: " + mouseX + ", Y: " + mouseY);
		    if(mouseX >= 30 && mouseX <= 60 && mouseY >= 20 && mouseY <= 40) {
		    	translateY += translateLength; // UP.
		    } else if(mouseX >= 30 && mouseX <= 60 && mouseY >= 50 && mouseY <= 70) {
			    translateY -= translateLength; // DOWN
			} else if(mouseX >= 20 && mouseX <= 38 && mouseY >= 30 && mouseY <= 60) {
			    translateX += translateLength; // LEFT
			} else if(mouseX >= 50 && mouseX <= 70 && mouseY >= 30 && mouseY <= 60) {
			    // RIGHT
			    translateX -= translateLength;
			}
		} else if(button == LoadData) {
			loadMgr.setVisible(true);
	  }
	}

	/**
	 * Configuration of explorer parameters. 
	 * @param d
	 */
	public void getparams(String d) {
	    reader = createReader(d);
	    String s="";
	    try {
	    	s = reader.readLine();
	    } catch(Exception e) {
	    	System.err.println("Error while reading line.");
	    	e.printStackTrace();
	    }
	    String [] ss = s.split(" ");
	    int n = Integer.parseInt(ss[0]);
	    paramx = new String[n];
	    paramy = new String[n];

	    // read in parameters from file.
	    try {  
		    for (int i = 0; i < n; i++)
		    	paramx[i] = reader.readLine().replace(" ", ",");
		    for (int i = 0; i < n; i++)
		    	paramy[i] = reader.readLine().replace(" ", ",");
		} catch (Exception e) {
			System.out.println("Error getting parameters. ");
			e.printStackTrace();
		}    
	}

	/**
	 * TOBE added .
	 */
	public void convert() {
	    View v = views[cvid];
	    //Checks if there is a view in that slot, and if there isnt, it loads a new view from disk
	    if (views[cvid] == null) {
		    v = new View(cvid);
		    views[cvid] = v;
		    String line;
	  
		    float maxdense=0;
		    String seq;
		    seq = String.valueOf(cvid);
		    
		    if (cvid < 10) seq = "0" + seq;
			else return; 
	    
		    reader = createReader(dataDir +"/part-000" + seq);  
		    //System.out.println("Loading file: " + dataDir +"/part-000" + seq);
	  
		    float dense=0;
		    String[] parts;
		    int cnt = 0;
		    while(true) {
			    try {
				    line = reader.readLine();
				    if (line == null || line == "") break;
				    line = line.trim();
				    parts = line.split("\t");
				} catch (IOException e) {
				    System.out.println("Error parsing reading and parsing line. ");
					break;
				}
			    
			    //System.out.println( parts[0]);
			    String[] p2 = PApplet.split(parts[0], '|');
			    Point p = new Point(p2[1], p2[2], parts[1]);
	    
				if (p._dense > maxdense)
				maxdense = p._dense;
				
				dense += p._dense;  
				v.addPoint(p);
				cnt++;
			}
		    v.normalize(maxdense, dense);
		    //System.out.println("Total points: " + Integer.toString(cnt));
		}  
	    currentv = views[cvid];
	}

	/**
	 * next frame.
	 */
	public void forward() {
	    cvid += 1;
	    if (cvid >= maxframe) {
		    cvid = 0;
		}
	    // System.out.println("cvid: "+cvid);
	    scrollbar.setValueToTickNumber(cvid);
	    convert();
	}

	/**
	 * go to the previous frame.
	 */
	public void backward() {
	    cvid -= 1;
	    if (cvid < 0) {
		    cvid = maxframe - 1;
		}
	    //System.out.println("cvid: "+cvid);
	    scrollbar.setValueToTickNumber(cvid);
	    convert();  
	}

	/**
	 * draw the interface GUI.
	 */
	float a = 0;
	
	public void draw() { 
		background(176, 196, 222);
		//background(102, 102, 102);
		View v = currentv;
		//background(200);
		//int d =10;
		if (stop == false) 
		    forward();   
		pushMatrix();
		translate(translateX, translateY);    
		scale(scale_factor);
		convert();
		
		// draw for the view. 
		for (int i = 0; i < v._ps.size(); i++) {
			//System.out.println(_ps.size());
			Point p =(Point) v._ps.elementAt(i); 
			fill(p._clr.getRed(), p._clr.getGreen(), p._clr.getBlue());
			rect(p._x* resol + viewx, p._y * resol + viewy, resol, resol);
		} // end for.
		
		popMatrix(); 
		
		// rectangle selection. 
		if(mousePressed) {
			stroke(255,0,0);
			strokeWeight(5);
			//fill(255);
			noFill();
			rect(selectionStartX,selectionStartY,mouseX-selectionStartX,mouseY-selectionStartY);
			noStroke();
		}
	} // end draw.

	void drawbar(float x, float y, int h, int r, int dx, int dy) {
	    float siny =  (y + viewy) / r;
	    float sinx =  (x + viewx) / r;
	    Color red = new Color(255, 0, 0);
	    //Color blue = new Color(51, 102, 153);
	   
	    float xx = x + viewx + dx;
	    float yy = y + viewy + dy;
	   
	    fill(red.getRed(), red.getGreen(), red.getBlue());
	    noStroke();
	    rect(xx, yy, (float)4.0, (float)4.0);
	    stroke(0);
	    line(xx, yy, (float)(xx + sinx * h ), (float)(yy+siny*h));
	}
	
	// used for selection of areas.
	private float selectionStartX; 
	private float selectionStartY;
	
	/**
	 * When mouse pressed, then selection a the left top coordinate of the rectangle.
	 */
	public void mousePressed() {
		selectionStartX = mouseX;
		selectionStartY = mouseY;
	}
	public void mouseReleased() {
		// change scale_factor according to the size of rectangle.
		// scale(600 / (mouseX-selectionStartX), 680 / (mouseY-selectionStartY));
		// scale_factor = max(600 / (mouseX-selectionStartX), 680 / (mouseY-selectionStartY));

		int newx = (int) ((mouseX + selectionStartX) / 2);
		int newy = (int) ((mouseY + selectionStartY) / 2);
		
		int origx = 600 / 2; 
		int origy = 680 / 2; 
				
		//translateX += 10; // origx - newx; 
		//translateY -= 10; // origy - newy; 
		//scale_factor = max(origx / (mouseX + selectionStartX), origy / (mouseY + selectionStartY));
		scale_factor += 0.5;
		translateX =  newx - origx  - 100;// * scale_factor; 
		translateY =  newy - origy - 200;// * scale_factor;
	}
	/**
	 * 
	 * @param val
	 * @return
	 */
	public static Color myMap(float val) {
	    val = -val;
	    float mid = (float) 0.4;
	    float vv;
	    vv = PApplet.abs((float)val);
	    
	    if(val < 0) {
		    val = mid - (mid * vv);
		} else {
		    val = mid + ((1-mid) * vv);
		}
	    float r=0;
	    float b=0;
	    float g=0;
	    if(val < 0){val = 0;}
	    if(val > 1){val = 1;}
	    //System.out.println(val);

	    switch ((int)(val/.2)) {
		case 0:
		    r = 255;
		    b = (float) (0 + (255*(val%.2)/.2));
		    g = 0;
		    break;
		case 1:
		    r = (float) (255 - (255*(val%.2)/.2));
		    b = 255;
		    g = 0;
		    break;
		case 2:
		    r = 0;
		    b = 255;
		    g = (float) (0 + (200*(val%.2)/.2));
		    break;
		case 3:
		    r = 100;
		    b = (float) (255 - (255*(val%.2)/.2));
		    g = (float) (150 + (105*(val%.2)/.2));
		    break;
		case 4:
		    r = (float) (105 - (105*(val%.2)/.2));
		    b = 0;
		    g = (float) (105 - (105*(val%.2)/.2));
		    break;
		case 5:
		    r = 0;
		    b = 0;
		    g = 25;
		    break;
		}
	    //System.out.println("Test." + Integer.toString(r) + " " + Integer.toString(g) + " " + Integer.toString(b));
	    return new Color((int)r, (int)g, (int)b);
	}
}
