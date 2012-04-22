package com.cloud.vista;

import guicomponents.GButton;
import guicomponents.GWSlider;
import guicomponents.GCombo;
import guicomponents.GLabel;
import guicomponents.GSlider;
import guicomponents.GTextField;
import guicomponents.GVertSlider;
import guicomponents.GWindow;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
// import java.util.Vector;
import java.awt.Color; 
//import java.applet.*;

// import com.cloud.vista.cloudManager.Button;
// import com.cloud.vista.cloudManager.ImageButtons;

import processing.core.*;
// import processing.app.*;

/**
 * The cloud vista explore, client for exploring the vista system. 
 * @author Simon Guo. 
 *
 */
public class VistaExplorer extends PApplet {
	public VistaExplorer() {
		super();
	}
	
	private static final long serialVersionUID = 1L;

	// for calling of the processing public functions, e.g. fill, draw etc. 
	public static PApplet parent;
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

	static String dir = "C:/data/census_s1_10_1"; 
	Point [] points; 
	private BufferedReader reader;
	static int cvid;

	int startx, starty, endx, endy;
	static boolean stop = false;

	// button for visual control and job control. 
	static GButton ZoomIn, ZoomOut, TranslateButton, PlayButton, ForwardButton, BackButton;
	static GWSlider scrollbar;
	static GVertSlider ZoomButton;
	// ImageButtons button;

	// load data button and the parameter window.
	GButton LoadData;
	GWindow Parameter;
	
	// map and reduce progress bar.
	static GWSlider MapProgressbar, ReduceProgressbar;

	// Below are plut-in for the window of loading data
	GButton Load;
	GTextField Text_Resolution;
	GTextField Text_NumberFrame;
	GTextField Text_StepLength;
	GTextField Text_MaxSample;
	GTextField Text_SampleRate;
	GTextField Text_Exploration;
	GCombo option;
	
	//// by xu.

	static View[] views;

	/**
	 * Setup the explore environment.
	 * @param none.
	 * @return void.
	 */
	public void setup() {
		size(600, 680);
	    translateX = 0;
	    translateY = -100;
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
	   
	    ZoomIn = new GButton(this, "c:/data/zoomIn.jpg", 1, 40, 80, 10, 10);
	    ZoomOut = new GButton(this, "c:/data/zoomOut.jpg", 1, 38, 303, 15, 15);
	    TranslateButton = new GButton(this, "c:/data/RLUD.jpg", 1, 20, 20, 50, 50);
	    PlayButton = new GButton(this, "c:/data/play.jpg", 1, 290, 630, 15, 15);
	    ForwardButton = new GButton(this, "c:/data/forward.jpg", 1, 340, 630, 15, 15);
	    BackButton = new GButton(this, "c:/data/backward.jpg", 1, 240, 630, 15, 15); 
	    
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
	    getParameter(dir + "_params");
	  
	    views = new View[maxframe];
	    convert(); 
	    frameRate(5);
	    stop = true;
	    
	    updateProgress(); // update progress now.
	   
	    // downloading visual frame files from the web server. 
	    try {
			// hadoop status and progress request. 
			new backgroundjob().start();
	       
	    } catch (Exception e) {
	    	System.out.println("Error happened while downloading visual frame files from cloud web server."); 
	    }
	}
	
	/**
	 * run background job.
	 * @author simon
	 */
	class backgroundjob extends Thread {
		public void run() {
			manager.submitHadoopJob();
			manager.getVisualFiles();
		}
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
		    //System.out.println("cvid: "+cvid);
		}
	}

	/**
	 * events handler for buttons. 
	 * @param button
	 */
	public void handleButtonEvents(GButton button) {
	    float step_length = (ZoomButton.getMinValue() - ZoomButton.getMaxValue()) / 10;
	    // System.out.println("step_length: " + step_length);
	    
	    if (button == ZoomIn) {    
		    System.out.println("Value(): " + ZoomButton.getValue());    
		    float now = ZoomButton.getValue() - step_length + ZoomButton.getMinValue();  
		    System.out.println("now: " + now);
		    if(now > ZoomButton.getMinValue()) {
			    now = ZoomButton.getMinValue();
			}
		    //System.out.println("now: "+now);  
		    ZoomButton.setValue(now);
		    System.out.println("Value: " + ZoomButton.getValue());
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
			    PlayButton.setImages("c:/data/pause.jpg", 1);
			    stop = false;      
			    frameRate(1);
			} else {
			    PlayButton.setImages("c:/data/play.jpg", 1);
			    stop = true;
			    frameRate(5);
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
		    System.out.println("Entering Loading Window");
		    Parameter = new GWindow(this, "Load Data", 200,200,400,330,false, null);
		    Parameter.setActionOnClose(GWindow.CLOSE_WINDOW);    
		    Parameter.papplet.setBackground(176);
		    String[] choice = {"Census", "KDDCup"};
		    option = new GCombo(this,choice, 20, 170, 30, 80);
		    Load = new GButton(this, "Load", 300, 30, 50, 20);
		    Text_Exploration = new GTextField(this, "", 30, 50, 100, 250);
		    GLabel exploration = new GLabel(this, "Existing Exploration", 30, 20, 150);
		    GLabel label_option = new GLabel(this, "Datasets", 170, 10, 80);
		    GLabel Resolution = new GLabel(this, "Resolution",       170,  70,  100);
		    GLabel NumberFrame = new GLabel(this, "Number of Frames",170,  120,  100);
		    GLabel StepLength = new GLabel(this, "Step Length",      170,  170,  100);
		    GLabel MaxSample = new GLabel(this, "Max Sample Size",   170,  220,  100);
		    GLabel SampleRate=new GLabel(this, "Sample Rate",      170,  270,  100);
		    Text_Resolution = new GTextField(this,"",     270,  70, 80,15);
		    Text_NumberFrame = new GTextField(this, "",   270,  120, 80,15);
		    Text_StepLength = new GTextField(this,"",     270,  170, 80,15);
		    Text_MaxSample = new GTextField(this,"",      270,  220, 80,15);
		    Text_SampleRate = new GTextField(this, "",    270,  270, 80,15);
		    Parameter.add(Text_Exploration);
		    Parameter.add(exploration);
		    Parameter.add(label_option);
		    Parameter.add(option);
		    Parameter.add(Text_Resolution);
		    Parameter.add(Text_NumberFrame);
		    Parameter.add(Text_StepLength);
		    Parameter.add(Text_MaxSample);
		    Parameter.add(Text_SampleRate);
		    Parameter.add(Load);
		    Parameter.add(Resolution);
		    Parameter.add(NumberFrame);
		    Parameter.add(StepLength);
		    Parameter.add(MaxSample);
		    Parameter.add(SampleRate);
	  } else if(button == Load) {
	     ///////////////////////////////
	     ///////////////////////////////
	     //////////////////////////////
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
	    
		    reader = createReader(dir +"/part-000" + seq);  
		    //System.out.println("Loading file: " + dir +"/part-000" + seq);
	  
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
	    if (cvid<0) {
		    cvid = maxframe-1;
		}
	    //System.out.println("cvid: "+cvid);
	    scrollbar.setValueToTickNumber(cvid);
	    convert();  
	}

	/**
	 * draw the interface GUI.
	 */
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
