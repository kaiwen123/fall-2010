package com.cloud.vista;

import java.util.Timer;
import java.util.TimerTask; 
import org.apache.hadoop.conf.*;
import org.apache.hadoop.mapred.*;

public class MapRedProgressChecker {
	private static int checkInterval = 5; 
	
	public static int mapProgress = 0; 
	public static int reduceProgress = 0; 
	
	public static void main(String[] args) throws InterruptedException {
		//System.out.println("Starting Hadoop Manager.....");
		periodicCheck();
		return; 
	}
	
	/*
	 * @brief System status check. 
	 * @param none.
	 * @return void. 
	 */
	public static void periodicCheck() {
		Timer timer = new Timer(); 
		timer.schedule(new CheckTask(), 0, checkInterval * 1000);
	}
	
	static class CheckTask extends TimerTask {
		@Override
		public void run () {
			//System.out.println("Checking Status... ");
			MapRedProgressChecker.checkStatus();
	    	VistaExplorer.m_mapProgressbar.setValue(MapRedProgressChecker.mapProgress);
	    	VistaExplorer.m_reduceProgressbar.setValue(MapRedProgressChecker.reduceProgress);
		}
	}
	
	/*
	 * @brief Check the status of the hadoop cluster. s
	 * @param none. 
	 * @return none. 
	 */
	public static void checkStatus() {
		try {
			Configuration conf = new Configuration(); 
			conf.set("mapred.job.tracker", "130.108.28.157:54311");
			
			JobClient client = new JobClient(new JobConf(conf));
			
			// cluster statuses. 
//			System.out.println(client.getClusterStatus()); 
//			System.out.println(client.getClusterStatus(true));
//			System.out.println(client.getConf().get("mapred.reduce.number"));
//			System.out.println(client.getSystemDir());
//			System.out.println(client.getDefaultMaps());
//			System.out.println(client.getDefaultReduces());
			//client.getFs().copyToLocalFile("hdfs://user/ada/test", "/home/ada/.bash_profile");
			//System.out.println(client.hashCode());
			
			
			// job status. 
			JobStatus[] jobStatuses = client.getAllJobs();
			
			//System.out.println("Cluster Status: " + client.getClusterStatus().getMapTasks() + " Map tasks and " + 
				//	client.getClusterStatus().getReduceTasks() + " Reduce tasks.");
			
			for (JobStatus jobStatus : jobStatuses) {
				//String jobId = jobStatus.getJobID().toString();
				//System.out.println("For job " + jobId);
				
				//client.getSetupTaskReports(jobStatus.getJobID());
				//client.getCleanupTaskReports(jobStatus.getJobID());
							
				int runState = jobStatus.getRunState();
				switch (runState) {
				case JobStatus.SUCCEEDED:
					//System.out.println("Successful job --> " + jobId);
					break; 
					
				case JobStatus.RUNNING:
					// running progress of the job.
					//System.out.println("Running job --> " + jobId);
					mapProgress = (int) (jobStatus.mapProgress() * 100);
					reduceProgress = (int) (jobStatus.reduceProgress() * 100);
					
					System.out.println("Progress of map job --> : " + Integer.toString(mapProgress));
					System.out.println("Progress of red job --> : " + Integer.toString(reduceProgress));
					
					/*
					TaskReport[] mapReports = client.getMapTaskReports(jobStatus.getJobID());
					for (TaskReport r : mapReports) {
						TaskID taskId = r.getTaskID();
						System.out.println("Map task   : " + taskId);
						System.out.println("Status     : " + r.getCurrentStatus());
						System.out.println("State      : " + r.getState());
						System.out.println("Progress   : " + r.getProgress());
						System.out.println("Finish Time: " + r.getFinishTime());
					}
											
					TaskReport[] reduceReports = client.getReduceTaskReports(jobStatus.getJobID());
					for (TaskReport r : reduceReports) {
						TaskID taskId = r.getTaskID();
						System.out.println("Reduce task: " + taskId);
						System.out.println("Status     : " + r.getCurrentStatus());
						System.out.println("State      : " + r.getState());
						System.out.println("Progress   : " + r.getProgress());
						System.out.println("Finish Time: " + r.getFinishTime());
					}
					break; 
					*/
					
				case JobStatus.FAILED:
					//System.out.println("Failed Job. ");
					break; 
					
				case JobStatus.KILLED:
					//System.out.println("Killed Job. ");
					break; 
					
				case JobStatus.PREP:
					//System.out.println("Prepared job. ");
					break; 
					
				default:
					System.out.println("No job is running...");
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
