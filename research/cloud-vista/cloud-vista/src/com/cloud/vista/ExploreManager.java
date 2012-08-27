package com.cloud.vista;
import java.awt.BorderLayout;
import java.awt.FlowLayout;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import java.awt.Component;
import java.awt.GridLayout;
import javax.swing.JList;
import java.awt.TextArea;
import java.awt.TextField;
import java.awt.Button;
import java.awt.List;
import javax.swing.GroupLayout;
import javax.swing.GroupLayout.Alignment;
import javax.swing.BoxLayout;

import org.apache.tools.ant.util.FileUtils;

import com.jgoodies.forms.layout.FormLayout;
import com.jgoodies.forms.layout.ColumnSpec;
import com.jgoodies.forms.layout.RowSpec;
import com.jgoodies.forms.factories.FormFactory;
import java.awt.Label;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.ComponentOrientation;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.Toolkit;
import java.io.File;
import java.nio.file.FileSystem;
import java.util.Vector;

/**
 * This class is used to manage the explorations, and also will be responsible for submiting job 
 * to the hadoop system. 
 * @author simon guo.
 *
 */
public class ExploreManager extends JDialog {
	// widgets elements.
	Label Datasets;
	Label ExistingExplo;
	List DatasetsList;
	List ExistingExploList;
	Label Resolution;
	TextField ResolutionText;
	Label NumberFrame;
	TextField NumberFrameText;
	Label Length;
	TextField LengthText;
	Label MaxSize;
	TextField MaxSizeText;
	Label SampleRate;
	TextField SampleRateText;
	
	/**
	 * parameters. 
	 */
	private static String m_projectDir = "C:/Users/simon/workspace/cloud-vista/";
	public static String m_explorationDataDir = m_projectDir + "exploration_data/";	// local dir for storage of visual frame data. 
	public static String m_resourceDir = m_projectDir + "resources/";
//	public static dddDataSet[] m_existingDataSets = null;  
	// values. 

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			ExploreManager dialog = new ExploreManager();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public ExploreManager() {
		setIconImage(Toolkit.getDefaultToolkit().getImage(m_resourceDir + "exploration_manager.jpg"));
		setResizable(false);
		setModal(true);
		setAlwaysOnTop(true);
		setTitle("Exploration Manager");
		setBounds(100, 100, 395, 340);
		getContentPane().setLayout(new FormLayout(new ColumnSpec[] {
				FormFactory.RELATED_GAP_COLSPEC,
				FormFactory.DEFAULT_COLSPEC,
				FormFactory.RELATED_GAP_COLSPEC,
				FormFactory.DEFAULT_COLSPEC,
				FormFactory.RELATED_GAP_COLSPEC,
				FormFactory.DEFAULT_COLSPEC,
				FormFactory.RELATED_GAP_COLSPEC,
				FormFactory.DEFAULT_COLSPEC,
				FormFactory.RELATED_GAP_COLSPEC,
				FormFactory.DEFAULT_COLSPEC,
				FormFactory.RELATED_GAP_COLSPEC,
				FormFactory.DEFAULT_COLSPEC,
				FormFactory.RELATED_GAP_COLSPEC,
				FormFactory.DEFAULT_COLSPEC,
				FormFactory.RELATED_GAP_COLSPEC,
				FormFactory.DEFAULT_COLSPEC,
				FormFactory.RELATED_GAP_COLSPEC,
				ColumnSpec.decode("max(25dlu;default)"),},
			new RowSpec[] {
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,
				FormFactory.RELATED_GAP_ROWSPEC,
				FormFactory.DEFAULT_ROWSPEC,}));
		
		final Label Datasets = new Label("Datasets");
		getContentPane().add(Datasets, "2, 2");
		
		final Label ExistingExplo = new Label("Existing Exploration");
		getContentPane().add(ExistingExplo, "6, 2");
		
		final List DatasetsList = new List();
		DatasetsList.setMultipleMode(false);
		DatasetsList.add("census");
		DatasetsList.add("kddcup");
		DatasetsList.select(0);
		DatasetsList.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
		getContentPane().add(DatasetsList, "2, 4, 1, 12, default, fill");
		
		final List ExistingExploList = new List();
		ExistingExploList.setMultipleMode(false);
		ExistingExploList.add("Census.exp.1.1.1");
		ExistingExploList.add("Census.exp.1.1.2");
		ExistingExploList.select(0);
		
		getContentPane().add(ExistingExploList, "6, 4, 13, 1, fill, fill");
		
		Label exploreName = new Label("Exploration Name");
		getContentPane().add(exploreName, "6, 8");
		
		TextField exploreNameText = new TextField();
		exploreNameText.setText("census.exp.rr.1.1");
		getContentPane().add(exploreNameText, "8, 8, 11, 1");
		
		final Label resolution = new Label("Resolution");
		getContentPane().add(resolution, "6, 10");
		
		final TextField resolutionText = new TextField();
		resolutionText.setText("1000");
		getContentPane().add(resolutionText, "8, 10, 11, 1");
		
		final Label Length = new Label("Scale");
		getContentPane().add(Length, "6, 12");
		
		final TextField LengthText = new TextField();
		LengthText.setText("1");
		getContentPane().add(LengthText, "8, 12, 11, 1");
		
		final Label sampleRate = new Label("Sample Rate");
		getContentPane().add(sampleRate, "6, 14");
		
		final TextField sampleRateText = new TextField();
		sampleRateText.setText("0.05");
		getContentPane().add(sampleRateText, "8, 14, 11, 1");
		
		final Label numberFrame = new Label("Number of Frames");
		getContentPane().add(numberFrame, "6, 16");
		
		final TextField numberFrameText = new TextField();
		numberFrameText.setText("10");
		getContentPane().add(numberFrameText, "8, 16, 11, 1");
		
//		loading existing exploration from data. 
		Button Load = new Button("Load");
		Load.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent event) {
				System.out.print("Loading existing exploration: ");
				String expo = ExistingExploList.getItem(ExistingExploList.getSelectedIndex());
				System.out.println(expo);
//				VistaExplorer.m_cloudManager.submitHadoopJob();
				loadExploration(expo);
			}
		});
		
//		creating new exploration from given parameters.
		Button newExplore = new Button("New");
		newExplore.addMouseListener(new MouseAdapter(){
			@Override
			public void mouseClicked(MouseEvent event){
//				addToExistingExploration("test");
				ExistingExploList.add("test", 0);
				System.out.println("Creating new exploration.");
				System.out.println("Parameters:");
				System.out.println("Data set:   " + DatasetsList.getItem(DatasetsList.getSelectedIndex()));
				System.out.println("Resolution: " + resolutionText.getText());
				System.out.println("Num Frames: " + numberFrameText.getText());
				System.out.println("Length:     " + LengthText.getText());
				System.out.println("Sample Rate:" + sampleRateText.getText());
			}
		});
		
		getContentPane().add(Load, "2, 18");
		getContentPane().add(newExplore, "2, 16");
	}
	
	/**
	 * Add the expoName to the database of existing explorations, and this name will 
	 * be displayed in the list. 
	 * @param exploreName the name of exploration to add to the list. 
	 * @return void.
	 */
	public void addToExistingExploration(String exploreName) {
		ExistingExploList.add(exploreName);
	}
	
	/**
	 * remove exploration from the exploration database. 
	 * @param exploreName the exploration to delete. 
	 * @return void
	 */
	public void removeExploration(String exploreName) {
		m_existingExpos.remove(exploreName);
		ExistingExploList.remove(exploreName);
		CloudManager.removeExploFromServer(exploreName);
		return;
	}
	
	/**
	 * Load existing explorations, the exploration data will be stored in the 
	 * local file system. If the current exploration does not exist in the local
	 * file system, it will request to the hadoop cluster to obtain the clusters.
	 * 
	 * @param exploreName name of exploration. 
	 */
	public Exploration loadExploration(String exploreName) {
		System.out.println("Starting to load existing exploration.");
		if(exists(exploreName)) {
			Exploration explore = new Exploration();
			String fullName = m_explorationDataDir + exploreName; 
			explore.setExplorationLocalFolder(fullName);
			explore.buildVisualFrames();
			return explore; 
		} else {
			return null; 
		}
	}
	
	/**
	 * Create new exploration with given parameters. It will request computation
	 * of new visual frames from the hadoop cluster and fetch the hadoop created visual
	 * frame to the local directory and in the end will call the loadExploration function
	 * to build the visual frames in the memory and will be displayed. 
	 * @param expoParam The parameters for exploration. 
	 */
	public Exploration buildExploration(String exploreName) {
		System.out.println("Start to load exploration: " + exploreName);
		String scale = "1", op_type="RR", step_length="0.05", resolution = "1", ndim = "10", nsteps="10", others="";
		CloudManager.buildExploreAndFetchFile(exploreName, scale, op_type, step_length, resolution, ndim, nsteps, others);
		Exploration explore = loadExploration(exploreName);
		return explore; 
	}
	
	/**
	 * compute subset when selecting a region, this is similar to the buildExploration function, 
	 * except that subset parameters will be provided to the parameter called others. 
	 * @param exploreName name of exploration to build. 
	 * @return the built exploration. 
	 */
	public Exploration buildSubsetExploration(String exploreName) {
		// compute subset with existing data.
		String scale = "1", op_type="SS", step_length="0.05", resolution = "1", ndim = "10", nsteps="10";
		String xstart = "-100", xend="100", ystart="-100", yend="100";
		String others=xstart + ":" + xend + ":" + ystart + ":" + yend; 
		CloudManager.buildExploreAndFetchFile(exploreName, scale, op_type, step_length, resolution, ndim, nsteps, others);
		Exploration explore = loadExploration(exploreName);
		return explore;
	}
	
	/**************************************************************/
	// Management of explorations. 
	static Vector<String> m_existingExpos = new Vector<String>(); 
	/**
	 * This will list existing explorations. 
	 * @return
	 */
	public void listExistingExplorations() {
		for(int i = 0; i < m_existingExpos.size(); i++) {
			System.out.println("Exploration: " + m_existingExpos.elementAt(i));
		}
		return; 
	}
	
	/**
	 * Delete existing exploration, including deletion of the exploration name from 
	 * the exploration data base the all the related data with this exploration. 
	 * @param expoName Name of the exploration to delete. 
	 * @return void. 
	 */
	public void deleteExploration(String expoName) {
		// check if the exploration exists at all. 
		if(exists(expoName)) {
			// delete the expo name from the database. 
			m_existingExpos.removeElement(expoName);
			
			// delete the data from the local directories. 
			FileUtils.delete(new File(m_explorationDataDir + expoName));
		}
	}
	
	/**
	 * Check to see if named exploration exists or not. 
	 * @param expoName Name of the exploration. 
	 * @return true if exists and false otherwise. 
	 */
	public boolean exists(String expoName) {
		return m_existingExpos.contains(expoName);
	}
	
	/**
	 * Synchronize the explorations with the server. 
	 * The cloud server will return a list of existing explorations, and 
	 * the client will compare this list with the local existing explorations. 
	 * @return
	 */
	public boolean syncWithServer() {
		String[] explores = CloudManager.listExplorationsFromServer().split(":");
		for(int i = 0; i < explores.length; i++) {
			// check the existence of the explore on the local. 
			if(!exists(explores[i])) {
				addToExistingExploration(explores[i]);
				File f = new File(explores[i]); 
				if(!f.exists()) {
					CloudManager.downloadFileFromServer(explores[i]);
				}
			}
		}
		return true; 
	}
}
