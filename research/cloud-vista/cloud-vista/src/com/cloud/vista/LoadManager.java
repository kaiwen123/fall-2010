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
	// variable elements.
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
	Button Load;
	
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
		
		Datasets = new Label("Datasets");
		getContentPane().add(Datasets, "2, 2");
		
		Label ExistingExplo = new Label("Existing Exploration");
		getContentPane().add(ExistingExplo, "6, 2");
		
		List DatasetsList = new List();
		DatasetsList.setMultipleMode(false);
		DatasetsList.add("Census");
		DatasetsList.add("Kddcup");
		DatasetsList.select(0);
		DatasetsList.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
		getContentPane().add(DatasetsList, "2, 4, 1, 12, default, fill");
		
		List ExistingExploList = new List();
		ExistingExploList.setMultipleMode(false);
		ExistingExploList.add("Census.exp.1.1.1");
		ExistingExploList.add("Census.exp.1.1.2");
		ExistingExploList.select(0);
		
		getContentPane().add(ExistingExploList, "6, 4, 13, 1, fill, fill");
		
		Label Resolution = new Label("Resolution");
		getContentPane().add(Resolution, "6, 8");
		
		TextField ResolutionText = new TextField();
		ResolutionText.setText("1000");
		getContentPane().add(ResolutionText, "8, 8, 11, 1");
		
		Label NumberFrame = new Label("Number of Frames");
		getContentPane().add(NumberFrame, "6, 10");
		
		TextField NumberFrameText = new TextField();
		NumberFrameText.setText("10\r\n");
		getContentPane().add(NumberFrameText, "8, 10, 11, 1");
		
		Label Length = new Label("Step Length");
		getContentPane().add(Length, "6, 12");
		
		TextField LengthText = new TextField();
		LengthText.setText("0.05");
		getContentPane().add(LengthText, "8, 12, 11, 1");
		
		Label MaxSize = new Label("Max Sample Size");
		getContentPane().add(MaxSize, "6, 14");
		
		TextField MaxSizeText = new TextField();
		MaxSizeText.setText("10000");
		getContentPane().add(MaxSizeText, "8, 14, 11, 1");
		
		Label SampleRate = new Label("Sample Rate");
		getContentPane().add(SampleRate, "6, 16");
		
		TextField SampleRateText = new TextField();
		SampleRateText.setText("0.05");
		getContentPane().add(SampleRateText, "8, 16, 11, 1");
		
		Button Load = new Button("Load");
		Load.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent event) {
				System.out.println("Executing hadoop command. ");
				VistaExplorer.manager.submitHadoopJob();
			}
		});
		getContentPane().add(Load, "2, 18");
	}
	
	/**
	 * return selected dataset.
	 */
	public String getDataSet() {
		return DatasetsList.getSelectedItem();
	}
	
	public String getSelectedExplore() {
		return ExistingExploList.getSelectedItem();
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
