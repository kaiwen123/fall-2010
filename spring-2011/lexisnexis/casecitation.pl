#!/usr/bin/perl -w 

# This perl program is used to extract all the case citations from 
# full case text docs. The result from this script will be used as
# the input for the classification steps. 
# 
# The format of our output is a string like this:
# LNI::citation::casename

open(INPUT, "$ARGV[0]"); 
open(OUTPUT, ">citations"); 

# main block to process the docs. 
while(<INPUT>) {
    $lnistr = &getLNI(\$_);
    $lnistr =~ s/://g;
    # citation for representation. 
    if ($_ =~ /<courtcase:representation>(.*?)<\/courtcase:representation>/g) {
	$repstr = $1; 
	# get link citation and statutory citation. 
	&getLCitation($repstr);
	&getSCitation($repstr);
    }
    # citation for opinion. 
    if ($_ =~ /<courtcase:opinion[^>]*?>(.*)<\/courtcase:opinion>/) {
	$opinstr = $1; 
	# get link citation and statutory ciations. 
	&getLCitation($opinstr);
	&getSCitation($opinstr);
    }
}

# @brief get LNI for an issue document. Usually, each issue doc
# has its unique issue identifier, called LNI. This identifier is
# bounded by <lncr:persistentidentifider> and </lncr:persistentidentifier>.
# 
# @param $docref - Reference to a string which stores the whole issue 
# doc as a string, which is bounded by <lncr:doc> and </lncr:doc>
# @return LNI string for the input issue document. 
sub getLNI {
    my $docref = $_[0];
    my $docstr = $$docref;
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
    my $randtemp = $_[0];
    while ($randtemp =~ /((.{200})(<lnci:cite ID=\"([^\"]*?)\"[^>]*?normprotocol=\"lexsee\"[^>]*>(.*?)<\/lnci:cite>))/g) {
    $timepass = $2;
    $temp = $5;
    if ( $timepass =~ /lni=\"([A-Z0-9-]+)\"/){ 
	$actuallni = $1; $actuallni =~ s/-//g; 
    }
    else { 
	$actuallni = "";
    }
    $temp =~ s/<.*?>//g;
    $temp =~ s/<\/.*?>//g;
    print OUTPUT $lnistr.":".$actuallni.":".$temp."\n";
    }
    return ;
}

# @brief function to get the statutory citations. 
# @param string that contains the citation text. 
# @return the formated citation string, as follows: 
# S_(cite_num)::TokenID::Actual citations. 
sub getSCitation {
    while ($_  =~ /(<lnci:cite ID=\"([^\"]*?)\"[^>]*?normprotocol=\"lexstat\"[^>]*>(.*?)<\/lnci:cite>)/g) {
	$token = $2;
	$temp = $3;
	$temp =~ s/<.*?>//g;
	$temp =~ s/<\/.*?>//g;
	print OUTPUT $lnistr.":".$token.":".$temp."\n";
    }
    return; 
}

close INPUT;
close OUTPUT; 
