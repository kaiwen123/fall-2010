#!/usr/bin/awk
BEGIN {
    preresult = 0;
    filteredresult = 0; 
    ratio = 0.0;
}

/.t -RTree-/
{preresult = $4
#print preresult
}

/.t -exhaust-/
{filtedresult = $4;
    #print filtedresult;
    print filtedresult / preresult;

}

END {
    print "Hello"
    #print filteredresult / preresult
}