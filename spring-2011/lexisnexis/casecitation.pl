#!/usr/bin/perl -w 

# This perl program is used to extract all the case citations from 
# full case text docs. The result from this script will be used as
# the input for the classification steps. 
# 
# The format of our output is a string like this:
# LNI::citation::casename

open(INPUT, "$ARGV[0]"); 
#open(OUTPUT, ">citations"); 

#$sep = "::";

# main block to process the docs. 
while(<INPUT>) {
    $lnistr = &getLNI(\$_);
    $cite = &getLCitation(\$_);

    # citation for representation. 
    while ($docstr =~ /<courtcase:representation>(.*?)<\/courtcase:representation>/g) {
	$repstr = $1; 
	# get link citation and statutory citation. 

	# get the name of the citation. 
	print $repstr . "\n";
    }
    # citation for opinion. 
    while ($docstr =~ /<courtcase:opinion[^>]*?>(.*)<\/courtcase:opinion>/) {
	$opinionstr = $1; 
	print $opinionstr . "\n";
    }

    print $lnistr . "\n" . $cite . "\n";
    #$citeid = &getCitation();
    #$citename = &getCiateName(); 
}

# @brief get LNI for an issue document. Usually, each issue doc
# has its unique issue identifier, called LNI. This identifier is
# bounded by <lncr:persistentidentifider> and </lncr:persistentidentifier>.
# 
# @param $docref - Reference to a string which stores the whole issue 
# doc as a string, which is bounded by <lncr:doc> and </lncr:doc>
# @return LNI string for the input issue document. 
sub getLNI {
    $docref = $_[0];
    $docstr = $$docref;
    my $lnistr; 
    if ($docstr =~ '<lncr:persistentidentifier>(.*)<\/lncr:persistentidentifier>')
    {
	$lnistr = $1;
	$lnistr =~ s/\-//g;
	$lnistr =~ s/^(.*)$/$1:/;
    }
    return $lnistr;    
}

# @brief This is to get the link citations from issue document. 
# The link citation is bounded by <lnci:cite ID="", normprotocol="">
# and </lnci:cite>
# @param an input string containing the citation. 
# @return the link citation. 
sub getLCitation {
    $refstr = $_[0];
    
}

close INPUT;
