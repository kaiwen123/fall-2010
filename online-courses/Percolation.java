//import java.lang.IndexOutOfBoundsException;

/**
 * Simulate the percolation problem.
 */
public class Percolation {

    private WeightedQuickUnionUF m1quFind = null;
    private int m1N = 0, m1openCnt;
    private boolean[][] m1isOpen = null;

    // create N-by-N grid, with all sites blocked
    public Percolation(int N) {
        m1quFind = new WeightedQuickUnionUF(N * N);
        m1N = N;
        m1isOpen = new boolean[N][N];
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                m1isOpen[i][j] = false; // close site. 
            }
        }
    }

    // open site (row i, column j) if it is not already
    public void open(int i, int j) {
        if (i < 1 || i > m1N || j < 1 || j > m1N) {
            throw new java.lang.IndexOutOfBoundsException();
        }

        m1isOpen[i - 1][j - 1] = true; // open it. 
        m1openCnt++;

        if (i > 1 && isOpen(i - 1, j)) { // top 
            m1quFind.union(m1N * (i - 1) + (j - 1), m1N * (i - 2) + (j - 1));
        }

        if (i < m1N && isOpen(i + 1, j)) {    // bottom
            m1quFind.union(m1N * (i - 1) + (j - 1), m1N * (i) + (j - 1));
        }

        if (j > 1 && isOpen(i, j - 1)) { // left 
            m1quFind.union(m1N * (i - 1) + (j - 1), m1N * (i - 1) + j - 2);
        }

        if (j < m1N && isOpen(i, j + 1)) { // right. 
            m1quFind.union(m1N * (i - 1) + (j - 1), m1N * (i - 1) + j);
        }
    }

    // is site (row i, column j) open?
    public boolean isOpen(int i, int j) {
        if (i < 1 || i > m1N || j < 1 || j > m1N) {
            throw new java.lang.IndexOutOfBoundsException();
        }
        return m1isOpen[i - 1][j - 1];
    }

    // is site (row i, column j) full?
    public boolean isFull(int i, int j) {
        if (i < 1 || i > m1N || j < 1 || j > m1N) {
            throw new IndexOutOfBoundsException();
        }
        for (int m = 0; m < m1N; m++) {
            if (isOpen(i, j) && m1quFind.connected(m, m1N * (i - 1) + j - 1)) {
                return true;
            }
        }
        return false;
    }

    // does the system percolate?
    // if any of two site on the first and the last row have the same id,
    // then it percolates, otherwise it is not. 
    public boolean percolates() {
        for (int i = 0; i < m1N; i++) {
            for (int j = 0; j < m1N; j++) {
                int cur = (m1N - 1) * m1N + j;
                if (isOpen(1, i + 1) && isOpen(m1N, j + 1) && m1quFind.connected(i, cur)) {
                    return true;
                }
            }
        }
        return false;
    }
}