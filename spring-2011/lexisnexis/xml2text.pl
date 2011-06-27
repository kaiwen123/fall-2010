#!/usr/bin/perl -w
# @file xml2text.pl. 
# @brief convert the xml input file into text format, in this process the
# footnotes, headnotes and the citation metadata will be extracted.
# @func getLNI(), getLCitations(), getSCitations(), extractText(). 
# @author Vinayak & Simon Guo. 
# @revision 04/10/2011 created by Vinayak. 
# @revision 06/16/2011 updated comments by Simon Guo. 
# @revision 06/23/2011 added sentence cutting part by Simon Guo. 

# Counting the number of documents processed. 
$cnt = 1;

# ======================================================================
# Go over all input issue documents and extract footnotes, headnotes, 
# citation metadata and paragraph text for the input. 
# ======================================================================
MAINLOOP:while (<STDIN>) {
    # Eliminate noise and weird characters.
    # print "Processing doc " . $cnt . " ......\n";
    s/§/S/g;
    s/ //g;
    s/\$ /\$/g;
    s/\&amp\;/\&/g;

    # get lni string for the doc. 
    $lnistr = "";    
    &getLNI(\$_);
    
    # court:representation information extraction.
    if (/(<courtcase:representation>)(.*?)(<\/courtcase:representation>)/) {
	&extractText(\$2);
    }

    # court:opinion information extraction. 
    if (/(<courtcase:opinion[^>]*?>)(.*)(<\/courtcase:opinion>)/) {
	&extractText(\$2);
    }
    $cnt++;
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
# @return none. 
# ======================================================================
sub extractText {
    my $strref = $_[0];

    # Look for all citations with lni's involved.
    &getLCitations($strref);
    
    # Look for CA_Headnotes if its California document.
    &getHeadnotes($strref); 
    
    # $footnote = $caheadnote;
    &getFootnotes($strref);
    
    # Look for statutory citations.
    &getSCitations($strref);
    
    # Get text from the document. 
    &getText($strref);
}

# ==================================================
# @brief Get the LNI string from the document. This value is constant
# throughout the document. 
# @param $strref Reference to the issue document as a string. 
# @return The document LNI string. 
# ==================================================
sub getLNI {
    my $strref = $_[0];
    
    if ($$strref =~ '<lncr:persistentidentifier>(.*?)<\/lncr:persistentidentifier>')
    {
	$lnistr = $1;
	$lnistr =~ s/\-//g;
    }
}

# ==================================================
# @brief This routine helps in finding the Link citations. That is "A vs. B"
# kind of citations. This captures and records casereftokens in the
# file. And the format it captures is as follows:
# Lni-current document::L_(citation_number)::TokenID::LNI-target document:: \
# Actual citation 
# @param input string with citation docs; 
# @param lnistr which is the global variable.
# @return Link citation strings in the input string.
# ==================================================
sub getLCitations {
    my $strref = $_[0];
    my $i = 0;
    
    while ($$strref =~ /((.{200})(<lnci:cite ID=\"([^\"]*?)\"[^>]*?normprotocol=\"lexsee\"[^>]*>(.*?)<\/lnci:cite>))/g){
	my $timepass = $2;
	my $string = $3;
	my $id = $4;
	my $temp = $5;
	my $actuallni = ""; 
	if ( $timepass =~ /lni=\"([A-Z0-9-]+)\"/) { 
	    $actuallni = $1; 
	    $actuallni =~ s/-//g; 
	}
	$i++;
	$temp =~ s/<.*?>//g;
	$temp =~ s/<\/.*?>//g;
	$citation = $lnistr.":L_".$i."::".$id."::".$actuallni."::\t".$temp."\n";
	print "CASEREFS:" . $citation;
	$$strref =~ s/\Q$string/ L_$i /g; # $temp/;
    }
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
    my $strref = $_[0];
    my $j = 0;

    while ($$strref  =~ /(<lnci:cite ID=\"([^\"]*?)\"[^>]*?normprotocol=\"lexstat\"[^>]*>(.*?)<\/lnci:cite>)/g)
    {
	$j++;
	my $string = $1;
	my $token = $2;
	my $temp = $3;
	$temp =~ s/<.*?>//g;
	$temp =~ s/<\/.*?>//g;
	print "CASEREFS:" . $lnistr.": S_".$j."::$token\t$temp\n";
	$$strref =~ s/\Q$string/ S_$j /g; # $temp/;
    }
}

# ==================================================
# @brief extract headnotes from document.
# @param 
# @return
# ==================================================
sub getHeadnotes {
    my $headref = $_[0];

    # Look for HNs for all other documents.
    while ($$headref =~ /(<casesum:headnote headnotesource=\"lexis-caselaw-editorial\">)(.*?)(<\/casesum:headnote>)/g) {
	$temp = $2;
	if ($temp =~ /(<text>)(.*?)(<\/text>)/) {
	    $i++;
	    $string = $2;
	    $string =~ s/<.*?>//g;
	    $string =~ s/<\/.*?>//g;
	    $mystring = "$lnistr:HN_$i $string\n\n";
	    print "HEADOUTPUT:" . $mystring;	
	}
    }
    my $i = 0;
    while ($$headref =~ /(<casesum:headnote headnotesource=\"lexis-caselaw-editorial\">)(.*?)(<\/casesum:headnote>)/g) {
	$i++;
	$$headref =~ s/(<casesum:headnote headnotesource=\"lexis-caselaw-editorial\">)(.*?)(<\/casesum:headnote>)/HN_$i /;
    }
    
    while ($$headref =~ /((<casesum:headnote headnotesource=\"ca-official-reporter\">)(.*?)(<ref:anchor id=\"hnpara_([0-9]+)\"\/>)(.*?)(<\/casesum:headnote>))/g) {
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
    
    while ($$headref =~ /((<casesum:headnote-ref.*?\">).*?<label>(.*?)<\/label>.*?(<\/casesum:headnote-ref>))/g) {	
	$string = $3;
	$$headref =~ s/((<casesum:headnote-ref.*?\">).*?(<\/casesum:headnote-ref>))/CAL_HN_$string /;
    }
}

# ==================================================
# @brief extract footnotes from document.
# @param 
# @return
# ==================================================
sub getFootnotes {
    my $footref = $_[0];

    # Look for all the footnotes within the document.
    while ($$footref =~ /(<footnote>)(.*?)(<\/footnote)/g) {
	if ($2 =~ /(<text>)(.*?)(<\/text>)/)	{
	    $i++;
	    $str = $2;
	    $str =~ s/<.*?>//g;
	    $str =~ s/<\/.*?>//g;
	    $str =~ s/  +/ /g;
	    $str =~ s/\( +/\(/g;
	    
	    if ( $str =~ / \" / ) {
		$str =~ s/ \" ([^"]+\S)\"/ \"$1\"/g ;
		$str =~ s/ \"(\S[^"]+) \" / \"$1\" /g ;
		$str =~ s/ \" ([^"]+) \" / \"$1\" /g ;
	    }
	    print "FOOTOUTPUT:" . $lnistr.":FN_".$i." ".$str."\n\n";
	}
    }
    
    $i=1;
    while ($$footref =~ /(<footnote>)(.*?)(<\/footnote>)/g) {
	$$footref =~ s/(<footnote>)(.*?)(<\/footnote>)/FN_$i /;	
	$i++;
    }
}

# ==================================================
# @brief extract text from the document. 
# @param 
# @return
# ==================================================
sub getText {
    my $i=1;
    my $strref = $_[0];
    my $parstr = "";
    
    # This part does the actual paragraph extraction.
    while  ($$strref =~ /<p>(.*)<\/p>/g) {
	while ($1 =~ /<text>(.*?)<\/text>/g) {
	    $partxt = $1;
	    $partxt =~ s/<.*?>//g;
	    $partxt =~ s/<\/.*?>//g;

	    # Check these cases, it might fail in cases other than
	    # sentences which start with a lower case letter. 
	    # as I am presuming that all such sentences which start with lower
	    # case can be attached to the previos paragraph.
	    if ($partxt =~ /^[a-z]+?/) { 
		$parstr =~ s/\s*\n*$//; 
		$parstr = $parstr." ".$partxt."\n\n"; 
	    } else {
		$parstr = $parstr."PARAGRAPH_".$k."\n".$partxt."\n\n";
		$k++;
	    }
	    $i++;
	}
    }

    $parstr = "\n\n\n".$lnistr."\n\n".$parstr;

    # Remove double spaces and other unimportant things.
    $parstr =~ s/  +/ /g;
    $parstr =~ s/\( +/\(/g;
    $parstr =~ s/ \./\./g;

    if ( $parstr =~ / \" / ) {
	$parstr =~ s/ \" ([^"]+\S)\"/ \"$1\"/g ;
	$parstr =~ s/ \"(\S[^"]+) \" / \"$1\" /g ;
	$parstr =~ s/ \" ([^"]+) \" / \"$1\" /g ;
    }

    # This part tries to avoid lni string being printed twice.
    # If a document has both courtcase:representation and courtcase:opinion parts
    # I tried to pass them both through this entire sub routine twice.
    # This part of logic avoids this LNI of document to be printed twice.
    if ($parstr !~ /^[\s\n\r]*[A-Z0-9]{23}:[\s\r\n]*$/) {
	print $parstr; 
    }    
}
