#!/usr/bin/perl -w 

# This perl program is used to extract all the case citations from 
# full case text docs. The result from this script will be used as
# the input for the classification steps. 
# 
# The format of our output is a string like this:
# LNI::citation::casename

#open(INPUT, "$ARGV[0]"); 
open(OUTPUT, ">citations"); 

# main block to process the docs. 
while(<STDIN>) {
    my $lnistr = &getLNI(\$_);
    $lnistr =~ s/://g;
    # citation for representation. 
    if ($_ =~ /<courtcase:representation[^>]*?>(.*?)<\/courtcase:representation>/g) {
	my $repstr = $1; 
	# get link citation and statutory citation. 
	&getLCitation($repstr, $lnistr);
	&getSCitation($repstr, $lnistr);
    }
    # citation for opinion. 
    if ($_ =~ /<courtcase:opinion[^>]*?>(.*?)<\/courtcase:opinion>/) {
	my $opinstr = $1; 
	# get link citation and statutory ciations. 
	&getLCitation($opinstr, $lnistr);
	&getSCitation($opinstr, $lnistr);
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
    my $lnistr = ""; 
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
    my ($randtemp, $lnistr) = @_;
    my $i = 0;
    while ($randtemp =~ /((.{200})(<lnci:cite ID=\"([^\"]*?)\"[^>]*?normprotocol=\"lexsee\"[^>]*>(.*?)<\/lnci:cite>))/g) {
    my $timepass = $2;
    my $string = $3; 
    my $id = $4;
    my $temp = $5;
    if ( $timepass =~ /lni=\"([A-Z0-9-]+)\"/){ 
	$actuallni = $1; $actuallni =~ s/-//g; 
    }
    else { 
	$actuallni = "";
    }
    $i++; 
    $temp =~ s/<.*?>//g;
    $temp =~ s/<\/.*?>//g;
    $citation = $lnistr.":L_".$i."::".$id."::".$actuallni."::".$temp."\n";
    print OUTPUT $citation;
    }
    return ;
}

# @brief function to get the statutory citations. 
# @param string that contains the citation text. 
# @param $lnistr The lni string from other source. 
# @return the formated citation string, as follows: 
# S_(cite_num)::TokenID::Actual citations. 
sub getSCitation {
    my ($citestr, $lnistr) = @_; 
    my $j = 0; 
    while ($citestr =~ /(<lnci:cite ID=\"([^\"]*?)\"[^>]*?normprotocol=\"lexstat\"[^>]*>(.*?)<\/lnci:cite>)/g) {
	$j++; 
	my $string = $1; 
	my $token = $2;
	my $temp = $3;
	$temp =~ s/<.*?>//g;
	$temp =~ s/<\/.*?>//g;
	$citation = $lnistr.":S_".$j."::".$token."::".$temp."\n";
	print OUTPUT $citation;
    }
    return; 
}

# close INPUT;
close OUTPUT; 
