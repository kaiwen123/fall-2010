
import java.util.Random;

/**
 * Get the percolation statistics.
 */
public class PercolationStats {

    private double[] m1fracs = null;
    private double m1mean = 0.0;
    private double m1stderr = 0.0;

    // perform T independent computational experiments on an N-by-N grid
    public PercolationStats(int N, int T) {
        if (N <= 0 || T <= 0) {
            throw new java.lang.IllegalArgumentException();
        }
        // monte carlo simulation.
        m1fracs = new double[T];
        Random rnd = new Random();
        int[][] locs = new int[2][N];

        for (int i = 0; i < N; i++) {
            locs[0][i] = i + 1;
            locs[1][i] = i + 1;
        }

        // random shuffle. 
        for (int i = 0; i < N; i++) {
            int x = rnd.nextInt(N);
            int tmp = locs[0][x];
            locs[0][x] = locs[0][N - x - 1];
            locs[0][N - x - 1] = tmp;
            x = rnd.nextInt(N);
            tmp = locs[1][x];
            locs[1][x] = locs[1][N - x - 1];
            locs[1][N - x - 1] = tmp;
        }

        // do a number of experiments. 
        for (int cnt = 0; cnt < T; cnt++) {
            int openCnt = 0;
            Percolation percol = new Percolation(N);
            for (int i = 0; i < N; i++) {
                try {
                    if (percol.isOpen(locs[0][i], locs[1][i])) {
                        percol.open(locs[0][i], locs[1][i]);
                        openCnt++;
                    }
                    if (percol.percolates()) {
                        break;
                    }
                } catch (IndexOutOfBoundsException e) {
                    continue;
                } //finally { ; }
            }
            m1fracs[cnt] = (double) openCnt / N / N;
            m1mean += m1fracs[cnt] / cnt;
        }

        // standard error.
        for (int i = 0; i < T; i++) {
            m1stderr += (m1fracs[i] - m1mean) * (m1fracs[i] - m1mean) / (T - 1 + 0.0001);
        }
    }

    // sample mean of percolation threshold
    public double mean() {
        return m1mean;
    }

    // sample standard deviation of percolation threshold
    public double stddev() {
        if (m1stderr == 0.0) {
            return m1stderr + 0.001;
        } else {
            return m1stderr;
        }
    }

    // test client, described below
    public static void main(String[] args) {
        int N = Integer.parseInt(args[0]);
        int T = Integer.parseInt(args[1]);

        Stopwatch sw = new Stopwatch();

        PercolationStats percol = new PercolationStats(N, T);

        double t = sw.elapsedTime();

        // 95% confidence interval of mean estimation. 
        double lower = percol.mean() - 1.96 * Math.sqrt(percol.stddev()) / Math.sqrt(T);
        double upper = percol.mean() + 1.96 * Math.sqrt(percol.stddev()) / Math.sqrt(T);

        System.out.println("time used               = " + t + "s");
        System.out.println("mean                    = " + percol.mean());
        System.out.println("stddev                  = " + percol.stddev());
        System.out.println("95% confidence interval = " + lower + "," + upper);
    }
}