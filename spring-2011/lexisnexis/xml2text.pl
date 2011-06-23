#!/usr/bin/perl -w
# @file xml2text. 
# @brief convert the xml input file into text format, in this process the
# footnotes, headnotes and the citation metadata will be extracted and
# stored onto files. 
# @author Vinayak & Simon Guo. 
# @revision 04/10/2011 created by Vinayak. 
# @revision 06/16/2011 updated comments by Simon Guo. 
# @revision 06/23/2011 added sentence cutting part by Simon Guo. 

# The number of documents processed. 
$count = 0;

# ======================================================================
# Go over all input issue documents and extract footnotes, headnotes, 
# citation metadata and paragraph text for the input. 
# ======================================================================
MAINLOOP:while (<STDIN>) {
    # Eliminate noise and weird characters.
    s/§/S/g;
    s/ //g;
    s/\$ /\$/g;
    s/\&amp\;/\&/g;
    
    my $lnistr = &getLNI(\$_);
    
    # court:representation information extraction.
    if ($_ =~ /(<courtcase:representation>)(.*?)(<\/courtcase:representation>)/) {
	$localstring = $2;
	&getFinishedProduct($localstring, $lnistr);
    }
    
    # courtcase:opinion information extraction.
    if ($_ =~ /(<courtcase:opinion[^>]*?>)(.*)(<\/courtcase:opinion>)/) {
	$localstring = $2;
	&getFinishedProduct($localstring, $lnistr);
    }

    $count++;
}

# ==================================================
# @brief Get the LNI string from the document. This value is constant
# throughout the document. 
# @param $strref Reference to the whole issue document. 
# @return The LNI string. 
# ==================================================
sub getLNI {
    my $strref = $_[0];
    my $lnistr = "";
    
    if ($$strref =~ '<lncr:persistentidentifier>(.*?)<\/lncr:persistentidentifier>')
    {
	$lnistr = $lnistr.$1;
	$lnistr =~ s/\-//g;
	$lnistr =~ s/^(.*)$/$1:/;
    }
    return $lnistr;
}

# ==================================================
# @brief This routine helps in finding the Link citations. That is "A vs. B"
# kind of citations. This captures and records casereftokens in the
# file. And the format it captures is as follows:
# Lni-current document::L_(citation_number)::TokenID::LNI-target document::
# Actual citation 
# @param input string with citation docs; 
# @param lnistr which is the global variable.
# @return Link citation strings in the input string.
# ==================================================
sub getLCitations {
    my ($docstrref, $lnistr) = @_;
    my $i = 0;
    
    while ($docstrref =~ /((.{200})(<lnci:cite ID=\"([^\"]*?)\"[^>]*?normprotocol=\"lexsee\"[^>]*>(.*?)<\/lnci:cite>))/g){
	my $timepass = $2;
	my $string = $3;
	my $id = $4;
	my $temp = $5;
	
	if ( $timepass =~ /lni=\"([A-Z0-9-]+)\"/) { 
	    $actuallni = $1; $actuallni =~ s/-//g; 
	} else { 
	    $actuallni = ""; 
	}
	$i++;
	$temp =~ s/<.*?>//g;
	$temp =~ s/<\/.*?>//g;
	$citation = $lnistr.":L_".$i."::".$id."::".$actuallni."::\t".$temp."\n";
	print "CASEREFS:" . $citation;
	$docstrref =~ s/\Q$string/ L_$i /; # $temp/;
    }
    return $docstrref."\n";
}

# ==================================================
# @brief As compared with the above routine, this routine helps in finding
# statutory citations in the document. The format it captures is as follows:
# Lni-current document::S_(citation_number)::TokenID:: Actual citation
# @param (document) string that contains the statutory citations.
# @param lnistr - the document unique string. 
# @return the statutory citation string. 
# ==================================================
sub getSCitations {
    my $j = 0;
    my ($actstr, $lnistr) = @_;
    while ($actstr  =~ /(<lnci:cite ID=\"([^\"]*?)\"[^>]*?normprotocol=\"lexstat\"[^>]*>(.*?)<\/lnci:cite>)/g)
    {
	$j++;
	my $string = $1;
	my $token = $2;
	my $temp = $3;
	$temp =~ s/<.*?>//g;
	$temp =~ s/<\/.*?>//g;
	print "CASEREFS:" . $lnistr.": S_".$j."::$token\t$temp\n";
	$actstr =~ s/\Q$string/ S_$j /; # $temp/;
    }
    return $actstr;
}

# ======================================================================
# @brief This routine calls citation and lnistr extraction routines to
# finish all the work. - Helper routine. 
# The idea is quite simple. The input string is an issue document. 
# Then I start replacing anything I find unimportant carrying the string
# till the end of the routine and by the time I reach the end, I have a well
# refined, paragraph separated routine which I can finally document.
# @param input string which is actually an issue document. 
# @param lnistr the documment unique lnistr. 
# @return 
# ======================================================================
sub getFinishedProduct {
    my ($randtemp, $lnistr) = @_;
    my $i = 0;
    my $finalstr = "";
    my $actstr = "";
    my $localstring = "";
    my $lcitation = "";

    # Look for all citations with lni's involved.
    $lcitation = &getLCitations($randtemp,$lnistr);

    my $caheadnote = "";
    my $footnote = "";

    # Look for CA_Headnotes if its California document.
    $headnote = $lcitation;

    # Look for HNs for all other documents.
    while ($headnote =~ /(<casesum:headnote headnotesource=\"lexis-caselaw-editorial\">)(.*?)(<\/casesum:headnote>)/g) {
	$temp = $2;
	if ($temp =~ /(<text>)(.*?)(<\/text>)/) {
	    $i++;
	    $string = $2;
	    $string =~ s/<.*?>//g;
	    $string =~ s/<\/.*?>//g;
	    $mystring = "$lnistr:HN_$i $string\n\n";
	    # select HEADOUTPUT;			
	    # $| = 1;
	    # print HEADOUTPUT  $mystring;	
	}
    }
    $i = 0;
    while ($headnote =~ /(<casesum:headnote headnotesource=\"lexis-caselaw-editorial\">)(.*?)(<\/casesum:headnote>)/g) {
	$i++;
	$headnote =~ s/(<casesum:headnote headnotesource=\"lexis-caselaw-editorial\">)(.*?)(<\/casesum:headnote>)/HN_$i /;
    }
    
    $caheadnote = $headnote;
    
    while ($caheadnote =~ /((<casesum:headnote headnotesource=\"ca-official-reporter\">)(.*?)(<ref:anchor id=\"hnpara_([0-9]+)\"\/>)(.*?)(<\/casesum:headnote>))/g) {
	$number = $5;
	$temp = $6;
	if ($temp =~ /(<text>)(.*?)(<\/text>)/)	{
	    $i = $number;
	    $string = $2;
	    $string =~ s/<.*?>//g;
	    $string =~ s/<\/.*?>//g;
	    
	    if ( $string =~ / \" / ) {
		$string =~ s/ \" ([^"]+\S)\"/ \"$1\"/g ;
		$string =~ s/ \"(\S[^"]+) \" / \"$1\" /g ;
		$string =~ s/ \" ([^"]+) \" / \"$1\" /g ;
	    }
	    print "HEADOUTPUT:" . "$lnistr:CAL_HN_$i $string\n\n";
	}
    }
    
    while ($caheadnote =~ /((<casesum:headnote-ref.*?\">).*?<label>(.*?)<\/label>.*?(<\/casesum:headnote-ref>))/g) {	
	$string = $3;
	$caheadnote =~ s/((<casesum:headnote-ref.*?\">).*?(<\/casesum:headnote-ref>))/CAL_HN_$string /;
    }
    
    $footnote = $caheadnote;

    $i = 0;
    # Look for all the footnotes within the document.
    while ($footnote =~ /(<footnote>)(.*?)(<\/footnote)/g) {
	$temp = $2;
	if ($temp =~ /(<text>)(.*?)(<\/text>)/)	{
	    $i++;
	    $string = $2;
	    $string =~ s/<.*?>//g;
	    $string =~ s/<\/.*?>//g;
	    $string =~ s/  +/ /g;
	    $string =~ s/\( +/\(/g;
	    
	    if ( $string =~ / \" / ) {
		$string =~ s/ \" ([^"]+\S)\"/ \"$1\"/g ;
		$string =~ s/ \"(\S[^"]+) \" / \"$1\" /g ;
		$string =~ s/ \" ([^"]+) \" / \"$1\" /g ;
	    }
	    
	    print "FOOTOUTPUT:" . $lnistr.":FN_".$i." ".$string."\n\n";
	}
    }
    
    $i=0;
    while ($footnote =~ /(<footnote>)(.*?)(<\/footnote>)/g) {
	$i++;
	$footnote =~ s/(<footnote>)(.*?)(<\/footnote>)/FN_$i /;	
    }
    $actstr = $footnote;

    # Look for statutory citations
    $actstr = &getSCitations($actstr,$lnistr);
    my $k=0;
    
    # This part does the actual paragraph extraction.
    while  ($actstr =~ /<p>(.*)<\/p>/g) {
	$temp = $1;
	while ($temp =~ /<text>(.*?)<\/text>/g) {
	    $i++;
	    $string = $1;
	    $string =~ s/<.*?>//g;
	    $string =~ s/<\/.*?>//g;
	    # Check these cases, it might fail in cases other than
	    # sentences which start with a lower case letter. 
	    # as I am presuming that all such sentences which start with lower
	    # case can be attached to the previos paragraph.
	    if ($string =~ /^[a-z]+?/) { 
		$finalstr =~ s/\s*\n*$//; $finalstr = $finalstr." ".$string."\n\n"; 
	    } else {
		$k++;  $finalstr = $finalstr."PARAGRAPH_".$k."\n".$string."\n\n";
	    }
	}
    }

    $finalstr = "\n\n\n".$lnistr."\n\n".$finalstr;

    # Remove double spaces and other unimportant things.
    $finalstr =~ s/  +/ /g;
    $finalstr =~ s/\( +/\(/g;
    $finalstr =~ s/ \./\./g;

    if ( $finalstr =~ / \" / ) {
	$finalstr =~ s/ \" ([^"]+\S)\"/ \"$1\"/g ;
	$finalstr =~ s/ \"(\S[^"]+) \" / \"$1\" /g ;
	$finalstr =~ s/ \" ([^"]+) \" / \"$1\" /g ;
    }

    # This part tries to avoid lni string being printed twice.
    # If a document has both courtcase:representation and courtcase:opinion parts
    # I tried to pass them both through this entire sub routine twice.
    # This part of logic avoids this LNI of document to be printed twice.
    if ($finalstr !~ /^[\s\n\r]*[A-Z0-9]{23}:[\s\r\n]*$/) {
	print STDOUT $finalstr; 
    }
}
