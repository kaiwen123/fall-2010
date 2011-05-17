import sys,random
from Cluster import Cluster

class WCD:
    def __init__(self, tf, k, capacity):
        self.ccnt = k           # Total number of clusters. 
        self.capacity = capacity
        self.tf = open(tf)
        self.itemset = set([])  # keep uniqueness. 
        n=0                     # number of occurrences.
        cnt =0                  # number of trans. 
        for line in self.tf:
            items = line.strip().split(' ')
            [self.itemset.add(i) for i in items]
            n += len(items)
            cnt +=1
        n /= cnt
        
        # Create k clusters and 
        # Create list for all items appear in the trans. 
        root = Cluster(0)
        root.leaf = False
        self.clusters = [root]
        self.itemset = list(self.itemset)        
        for i in range(2):
            c = Cluster(i)
            #c.rand_init(self.itemset,int(n) )
            self.clusters.append(c)
            c.pprint()

        #self.clusters[0].add_trans(['a','b','c'])
        #self.clusters[1].add_trans(['x','y','z'])
        self.members = []
        exit

    def phase1(self):
        ''' Add trans to cluster which can achieve 
        highest entropy increase. '''
        self.tf.seek(0)
        for line in self.tf:
            t = line.strip().split(' ')
            maxv = -1
            maxc = 0

            for i in range(len(self.clusters)):
                if self.clusters[i].leaf:
                    v = self.clusters[i].test_trans(0, t)
                    if maxv < v:
                        maxv = v
                        maxc = i
            # here need to check the capacity of the cluster.
            # if maximum capacity is achieved, do split. 
            if self.clusters[maxc].nk >= self.capacity:
                c = self.clusters[maxc].split()
                c.add_trans(t)
                self.clusters.append(c)
                del self.clusters[maxc]
                print 'cluster splitted' 
            else:
                self.clusters[maxc].add_trans(t)
            self.members.append(self.clusters[maxc].id)
            print self.clusters

    def phase2 (self, iter):
        for i in range(iter):
            self.tf.seek(0)
            j =0
            for line in self.tf:
                t = line.strip().split(' ')
                maxv = -1
                maxc = None
               
                cid = self.members[j]

                v0=self.clusters[cid].test_trans(1,t)
                m = 0
                mi = 0
                for c in self.clusters:
                    if c.id == self.members[j]:
                        continue
                    v = c.test_trans(0, t)
                    if maxv <v:
                        maxv = v
                        maxc = c
                if v0 < v:
                    self.clusters[cid].remove_trans(t)
                    
                    # test if maxc achieved the highest capacity. 
                    if maxc.nk >= self.capacity:
                        cnew1, cnew2 = maxc.split()
                        cnew2.add_trans(t)
                        self.clusters.append(cnew1)
                        self.clusters.append(cnew2)
                        del self.clusters[mi]
                        print 'added cluster', cnew1, 'and', cnew2
                        print 'deleted cluster', maxc
                    else:
                        maxc.add_trans(t)

                    self.members[j]=maxc.id
                j +=1 
   
                # self.pprint()
    def ptrans(self):
        ''' print clusters. ''' 
        result = open('result.txt', 'w')
        self.tf.seek(0);
        m = 0                   # line count. 
        for line in self.tf:
            #print self.members[m], line
            result.write(str(self.members[m]) + ' ' + line)
            m += 1
        result.close()

    def pprint(self):
        print "result:", len(self.members)
        print self.members
        print 'Cluster Result: '
        print 'id\titems\twcd'
        for c in self.clusters:
            c.pprint()
def main():
    w = WCD(sys.argv[1], 10, 20)
    w.phase1()
    # w.pprint()
    # w.phase2(5)
    # w.pprint()
    # w.ptrans()

if __name__=='__main__':
    main()       
