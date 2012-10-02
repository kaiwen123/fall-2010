/**
 * This program is to output the running time of some given
 * java program and fit the curve with math software. 
 */
public class AlgorithmAnalysis {
    public static void main(String[] args) {
	int startCnt = 64; 
	int stopCnt = 1024; 
	long seed = 689344; 
	Stopwatch sw = null;
	double t1 = 1.0, t2 = 0.0;
	
	System.out.printf("%10s%10s%10s%10s\n", "N", "seconds", 
			  "ratio", "log ratio");

	for (int n = startCnt; n < stopCnt; n *= 2) {
	    sw = new Stopwatch(); 
	    //Timing.trial(n, seed);
	    t2 = sw.elapsedTime();
	    double ratio = t2 / t1; 
	    double logRatio = Math.log(ratio) / Math.log(2);
	    System.out.printf("%10d%10.2f%10.2f%10.2f\n", n, t2, 
			      ratio, logRatio);
	    t1 = t2; 
	}
    }
}
