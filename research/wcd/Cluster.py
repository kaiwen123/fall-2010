import sys,random

class Cluster:
    def __init__(self, i):
        self.id = i
        self.items={}          # items and occurrences. e.g. 'a':10.
        self.sk =0             # total number of all item occurrences.
        self.nk =0             # number of transactions.
        #self.mk = 0 # number of distinct items
        self.wcd = 0.0          # Weighted cluster density.
        self.sk2 = 0.0          # square occurrences. 
        self.parent = 0         # the parent node of this cluster. 
	self.children = []      # children of this node. 
        self.leaf = True        # is current node leaf node? 
        print 'cluster initialized'

    def split(self, ctree, capacity):
        ''' Split cluster into two clusters. 
        cid is the id of the newly created cluster. 
        ctree is the cluster tree. 
        '''
        cid = len(ctree)        # put to the end of the list. 
        c = Cluster(cid)
        c.leaf = self.leaf
        c.parent = self.parent
        ctree[self.parent].children.append(cid)
        ctree.append(c)

        # TODO, check parent up to the root.
        return cid

    def add_trans(self, t):
        # t is a list of items
        # WCD is calculated with sk2/(sk*nk).
        print 'adding tran', t, 'to cluster', self.id, self.wcd
        for ti in t:
            self.items.setdefault(ti, 0)
            self.items[ti] += 1
            # (s+1)^2 = s^2 + 2*s + 1. recursive. 
            print self.items[ti]
            self.sk2 += 1+ 2*self.items[ti]
            
        self.sk += len(t)
        print '==========', self.sk, self.sk2 
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
        print 'Initialization success'
        random.shuffle(iset)
        items = iset[:l]
        self.add_trans(items)
        # None

    def pprint(self):
        print self.id, self.items, self.wcd

# Test onely.
def main():
    w = WCD(sys.argv[1], 10, 20)
    w.phase1()
    w.pprint()
    w.phase2(5)
    w.pprint()
    w.ptrans()

if __name__=='__main__':
    main()       
