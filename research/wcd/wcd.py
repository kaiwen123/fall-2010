import sys,random

class Cluster:
    def __init__(self, i):
        self.id = i
        self.items={}          # items and occurrences. e.g. 'a':10.
	self.children = set([]) # children. 
        self.sk =0             # total number of all item occurrences.
        self.nk =0             # number of transactions.
        #self.mk = 0 # number of distinct items
        self.wcd = 0.0          # Weighted cluster density.
        self.sk2 = 0.0          # square occurrences. 
        print 'cluster initialized..'

    def split_cluster(self):
        ''' Split cluster into two clusters. '''
        c1 = Cluster(str(self.id) + '1')
        c1.items = self.items
        c2 = Cluster(self.id + '2')
        self.children.add(c1)
        self.children.add(c2)

    def merge_cluster(self):
        ''' Merge two clusters into one. '''
        

    def add_trans(self, t):
        # t is a list of items
        # WCD is calculated with sk2/(sk*nk).
        print 'adding tran', t, 'to cluster', self.id, self.wcd
        for ti in t:
            self.items.setdefault(ti, 0)
            # (s+1)^2 = s^2 + 2*s + 1. recursive. 
            self.sk2 += 1+ 2*self.items[ti]
            self.items[ti] += 1
            
        self.sk += len(t)
        self.nk +=1
        self.wcd = self.sk2 / self.sk # error. 
        # self.wcd = self.sk2 / (self.sk * self.nk)
        
    def remove_trans(self, t):
        print 'removing tran:', t
        for ti in t:
            # s^2 = (s+1)^2 - 2*s -1.
            self.sk2 -= 1+2*self.items[ti]
            self.items[ti] -= 1
        self.sk -= len(t)
        self.nk -=1 
        self.wcd =0
        if self.sk!=0:
            self.wcd = self.sk2/self.sk
            # self.wcd = self.sk2 / (self.s  * self.nk)
 
    def test_trans(self, type, t):
        # return increased WCD
        sk2 =0.0;
        # print 'testing tran:', t
        for ti in t:
            if self.items.has_key(ti):
                sk2 += 1 + 2*self.items[ti]
            else:
                sk2 +=1

        if type == 0:           # add
            return (self.sk2+sk2)/(self.sk+len(t)) -  self.wcd # error.
            # return (self.sk2+sk2)/((self.sk+len(t))*(self.nk+1)) - self.wcd
        else:                   # remove 
            if self.sk -len(t)==0 or self.nk == 1:
                return 0
            else:
                return self.wcd - (self.sk2 - sk2)/(self.sk-len(t))

    def rand_init(self, iset, l):
        print 'Initialization success...'
        random.shuffle(iset)
        items = iset[:l]
        self.add_trans(items)
        # None

    def pprint(self):
        print self.id, self.items, self.wcd

class WCD:
    def __init__(self, tf, k, capacity):
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
        self.clusters = []
        self.itemset = list(self.itemset)        
        for i in range(k):
            c = Cluster(i)
            #c.rand_init(self.itemset,int(n) )
            self.clusters.append(c)
            c.pprint()

        #self.clusters[0].add_trans(['a','b','c'])
        #self.clusters[1].add_trans(['x','y','z'])
        self.members = []

    def phase1(self):
        ''' Add trans to cluster which can achieve 
        highest entropy increase. '''
        self.tf.seek(0)
        self.all = []
        for line in self.tf:
            t = line.strip().split(' ')
            self.all.append(t)
            maxv = -1
            maxc = self.clusters[0] # None # changed. 
            for c in self.clusters:
                v = c.test_trans(0, t)
                if maxv < v:
                    maxv = v
                    maxc = c
            # here need to check the capacity of the cluster.
            # if maximum capacity is achieved, do split. 
            if maxc.nk >= self.capacity:
                cl = maxc.split_cluster()
                cl.add_trans(t)
                
            else:
                maxc.add_trans(t)
            self.members.append(maxc.id)

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

                for c in self.clusters:
                    if c.id == self.members[j]:
                        continue
                    v = c.test_trans(0, t)
                    if maxv <v:
                        maxv = v
                        maxc = c
                if v0 < v:
                    self.clusters[cid].remove_trans(t)
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
    w.pprint()
    w.phase2(5)
    w.pprint()
    w.ptrans()

if __name__=='__main__':
    main()       
