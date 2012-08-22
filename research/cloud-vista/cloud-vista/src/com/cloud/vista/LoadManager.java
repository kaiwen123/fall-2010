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

/**
 * This class is used to manage the explorations, and also will be responsible for submiting job 
 * to the hadoop system. 
 * @author simon guo.
 *
 */
public class LoadManager extends JDialog {
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

	// values. 

	/**
	 * Launch the application.
	 */
	public static void main(String[] args) {
		try {
			LoadManager dialog = new LoadManager();
			dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
			dialog.setVisible(true);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Create the dialog.
	 */
	public LoadManager() {
		setIconImage(Toolkit.getDefaultToolkit().getImage("C:\\Users\\zhen\\workspace\\zip\\imagesCAFKUEQI.jpg"));
		setResizable(false);
		setModal(true);
		setAlwaysOnTop(true);
		setTitle("Cloud Manager");
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
		DatasetsList.add("Census");
		DatasetsList.add("Kddcup");
		DatasetsList.select(0);
		DatasetsList.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
		getContentPane().add(DatasetsList, "2, 4, 1, 12, default, fill");
		
		final List ExistingExploList = new List();
		ExistingExploList.setMultipleMode(false);
		ExistingExploList.add("Census.exp.1.1.1");
		ExistingExploList.add("Census.exp.1.1.2");
		ExistingExploList.select(0);
		
		getContentPane().add(ExistingExploList, "6, 4, 13, 1, fill, fill");
		
		final Label Resolution = new Label("Resolution");
		getContentPane().add(Resolution, "6, 8");
		
		final TextField ResolutionText = new TextField();
		ResolutionText.setText("1000");
		getContentPane().add(ResolutionText, "8, 8, 11, 1");
		
		final Label NumberFrame = new Label("Number of Frames");
		getContentPane().add(NumberFrame, "6, 10");
		
		final TextField NumberFrameText = new TextField();
		NumberFrameText.setText("10");
		getContentPane().add(NumberFrameText, "8, 10, 11, 1");
		
		final Label Length = new Label("Step Length");
		getContentPane().add(Length, "6, 12");
		
		final TextField LengthText = new TextField();
		LengthText.setText("0.05");
		getContentPane().add(LengthText, "8, 12, 11, 1");
		
		final Label MaxSize = new Label("Max Sample Size");
		getContentPane().add(MaxSize, "6, 14");
		
		final TextField MaxSizeText = new TextField();
		MaxSizeText.setText("10000");
		getContentPane().add(MaxSizeText, "8, 14, 11, 1");
		
		final Label SampleRate = new Label("Sample Rate");
		getContentPane().add(SampleRate, "6, 16");
		
		final TextField SampleRateText = new TextField();
		SampleRateText.setText("0.05");
		getContentPane().add(SampleRateText, "8, 16, 11, 1");
		
//		loading existing exploration from data. 
		Button Load = new Button("Load");
		Load.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent event) {
				System.out.println("Loading existing exploration: ");
				String expo = ExistingExploList.getItem(ExistingExploList.getSelectedIndex());
				System.out.println(expo);
//				VistaExplorer.manager.submitHadoopJob();
				VistaExplorer.loadExploration(expo);
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
				System.out.println("Resolution: " + ResolutionText.getText());
				System.out.println("Num Frames: " + NumberFrameText.getText());
				System.out.println("Length:     " + LengthText.getText());
				System.out.println("Max size   :" + MaxSizeText.getText());
				System.out.println("Sample Rate:" + SampleRateText.getText());
			}
		});
		getContentPane().add(Load, "2, 18");
		getContentPane().add(newExplore, "2, 16");
	}
	
	public void addToExistingExploration(String expo) {
		ExistingExploList.add(expo);
	}
	
	/**
	 * return selected dataset.
	 */
	public String getDataSet() {
		return DatasetsList.getItem(DatasetsList.getSelectedIndex());
	}
	
	public String getSelectedExplore() {
		return ExistingExploList.getItem(ExistingExploList.getSelectedIndex());
	}
	
	public String getResolution() {
		return ResolutionText.getText();
	}

	public String getNumFrames() {
		return NumberFrameText.getText();
	}
	
	public String getLength() {
		return LengthText.getText();
	}

	public String getMaxSize() {
		return MaxSizeText.getText();
	}
	
	public String getSampleRate() {
		return SampleRateText.getText();
	}
}
