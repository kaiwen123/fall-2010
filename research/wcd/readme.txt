Classes:
wcd - overall control class. 
members:
1, items // item occurance frequencies. 
2, members // the membership(only leaf) of each trans. 
3, cfroot // the root of the cftree. 
methods: 
1, phase1() // clustering. 
2, phase2() // adjustment of membership on the leaf node.
3, pprint() // print clustering result. 

--------------------------------------------------
cftree - maintaining the tree structure.
members:
1, root //root of tree. 
2, height // height of tree. 
3, fanout // max child node of a node.
4, maxheight // max height of tree. 
5, nodecap // max number of entries a node can host. 
methods:
1, insert_trans() // for phase1.  
1.1, split() // split node. 
2, adjust_trans() // for phase2. 

insert_trans() will start from the root node, it does following:
for each transaction. 
If current node is not leaf node, go to I1, else go to I3.
I1, update summary in current node, e.g. wcd etc. 
I2, choose a subtree among all the children for current node, 
    which can have maximum wcd increment.
    if the chosen subtree is not leaf node, then go to I1. 
    else if subtree is leaf node, then go to I3. 
I3, compare the wcd increment over all the entries in the leaf node
    and the increment by adding a new entry to current node. And 
    choose the entry with maximum wcd increment. 
    if the chosen entry exists, insert trans to this entry. 
    if the chosen entry is a newly created one, then
    create the entry insert the trans into the new entry and put the 
    new entry into the current node. 
    Test the size of the current node. 
    if it is not overflow (m+1), then return. 
    else go to the split() process. and return. 
I4, split leaf node. 

split() is used to create a new node and increment the tree. 
The to be splitted node should be leaf node. if not, return false.
S1, create a new node. 
S2, setup the parent of the new node. 
S3, partition the entries or the child nodes between the to be 
    splitted node and the new node. 
S4, go the parent node of the current splitted node, 
    if this node need split, then go to S1, 
    else split is done. 

adjust_trans() is used for phase2, the cluster adjustment process.
for each transaction, it does the following thing:
1, get the current entry membership for current trans. 
2, locate the node where this entry reside in. ??
3, test over all the entries on this transaction, and find the one 
   with maximum wcd increment. insert the node into this entry, and
   at the same time erase it from the previous entry. 

--------------------------------------------------
cfnode - the node in the tree, each node contains at least one	
       entry, for nonleaf node it contains only one, and for 
       leaf node, it contain multiples of. 
members:
1, entries // all the entries in this node. 
2, children // pointers to the childrens. 
3, parent // parent for this node. 
4, level  // level of current node.
methods:
1, partition() // partition entries in this node with another node. 
2, isleaf
3, isOverflow() // over flow or not. 
   isIndexOverflow()
   isLeafOverflow()
4, containsEntry() // if the node contains a specific entry. 
5, getSummaryWcd() // sum wcd of all entries. 

--------------------------------------------------
entry - summary information object. 
members:
1, summary variable.
methods:
1, add_trans() // update summary info for adding trans. 
2, remove_trans() // update summary info for removing trans. 
3, test_trans() // test the wcd increment by adding or removing trans. 
4, print() 
