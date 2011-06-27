#!/usr/bin/perl -w
# @file xml2text.pl. 
# @brief convert the xml input file into text format, in this process the
# footnotes, headnotes and the citation metadata will be extracted.
# @func getLNI(), getLCitations(), getSCitations(), extractText(). 
# @author Vinayak & Simon Guo. 
# @revision 04/10/2011 created by Vinayak. 
# @revision 06/16/2011 updated comments by Simon Guo. 
# @revision 06/23/2011 added sentence cutting part by Simon Guo. 
# @revision 06/27/2011 added detailed comments. 

# Counting the number of documents processed. 
$cnt = 1;

# ======================================================================
# @brief this is the main loop over all the legal documents. It will go 
# over all input legal issue documents(usually, each line is a legal doc)
# and extract footnotes, headnotes, citation metadata and paragraph text.
# 
# In order to use the power of distributed computing such as hadoop, I 
# used <STDIN> as input to the program and <STDOUT> as output file. 
# 
# @input <STDIN>, each line of input is an issue document in xml format. 
# @output <STDOUT>, the information we need will include footnote, headnote, 
# citation meta-data and paragraph/sentences. When these info is printed 
# to STDOUT, a unique identifier will be prepended to the string. They are
# FOOTNOTES:, HEADNOTES:, CITATION. And we can process the data according 
# to these info for the following steps. 
# 
# @var Global variables : 
# $lnistr - the doc unique doc as string; 
# $docstr - courtcase representation string; 
# 
# @comments Due to the large size of the input legal document, it is 
# advisible to pass reference to function by reference rather than by 
# value. And $lnistr is treated as a global variable such that all the 
# functions in the script can use it.
# ======================================================================
MAINLOOP:while (<STDIN>) {
    # Eliminate noise and weird characters.
    s/§/S/g;			# remove special chars; 
    s/ //g;
    s/\$ /\$/g;
    s/\&amp\;/\&/g;		# transform ascii chars to normal form. 

    # a global string variable. 
    $docstr = ""; 
    $lnistr = "";

    # get lni string for the doc. 
    &getLNI(\$_);
    
    # court:representation information extraction.
    if (/<courtcase:representation>(.*?)<\/courtcase:representation>/) {
	$docstr = $1; 
    }

    # court:opinion information extraction. 
    if (/<courtcase:opinion[^>]*?>(.*)<\/courtcase:opinion>/) {
	$docstr .= " " . $1;
    }
    
    # Extract text and meta-data. 
    &extractText();
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
    # Look for all link citations with lni's involved.
    &getLCitations();

    # Look for CA_Headnotes if its California document.
    &getHeadnotes(); 

    # $footnote = $caheadnote;
    &getFootnotes();

    # Look for statutory citations.
    &getSCitations();
    
    # Get text from the document. 
    &getText();

    return;    
}

# ==================================================
# @brief Get the LNI string from the document. This value is constant
# throughout the document. 
# @param reference to the document as a xml string. 
# @return The document LNI string. 
# ==================================================
sub getLNI {
    my $docref = $_[0]; 
    if ($$docref =~ '<lncr:persistentidentifier>(.*?)<\/lncr:persistentidentifier>')
    {
	$lnistr = $1;
	$lnistr =~ s/\-//g;
    }
    return;
}

# ==================================================
# @brief This routine helps in finding the Link citations. That is "A vs. B"
# kind of citations. This captures and records casereftokens in the
# file. And the format it captures is as follows:
# Lni-current document::L_(citation_number)::TokenID::LNI-target document:: \
# Actual citation 
# @param $docstr  which are global variables. 
# @param lnistr which is the global variable.
# @return Link citation strings in the input string.
# ==================================================
sub getLCitations {
    my $i = 1;
    
    while ($docstr =~ /((.{200})(<lnci:cite ID=\"([^\"]*?)\"[^>]*?normprotocol=\"lexsee\"[^>]*>(.*?)<\/lnci:cite>))/g){
	my $timepass = $2;
	my $string = $3;
	my $id = $4;
	my $temp = $5;
	my $actuallni = ""; 
	if ( $timepass =~ /lni=\"([A-Z0-9-]+)\"/) { 
	    $actuallni = $1; 
	    $actuallni =~ s/-//g; 
	}
	$temp =~ s/<.*?>//g;
	$temp =~ s/<\/.*?>//g;
	$citation = $lnistr.":L_".$i."::".$id."::".$actuallni."::\t".$temp."\n";
	print "CASEREFS:" . $citation;
	$docstr =~ s/\Q$string/ L_$i /g; # $temp/;
	$i++;
    }
}

# ==================================================
# @brief As compared with the above routine, this routine helps in finding
# statutory citations in the document. The format it captures is as follows:
# Lni-current document::S_(citation_number)::TokenID:: Actual citation
# @param $docstr which are global variables for each doc. 
# @param lnistr - the document unique string. 
# @return none, but will update the global $docstr. 
# ==================================================
sub getSCitations {
    my $i = 1;

    while ($docstr  =~ /(<lnci:cite ID=\"([^\"]*?)\"[^>]*?normprotocol=\"lexstat\"[^>]*>(.*?)<\/lnci:cite>)/g)
    {
	my $string = $1;
	my $token = $2;
	my $temp = $3;
	$temp =~ s/<.*?>//g;
	$temp =~ s/<\/.*?>//g;
	print "CASEREFS:" . $lnistr.": S_".$i."::$token\t$temp\n";
	$docstr =~ s/\Q$string/ S_$i /g; # $temp/;
	$i++;
    }
}

# ==================================================
# @brief extract headnotes from document.
# @param $docstr, a global variable. 
# @return none. 
# ==================================================
sub getHeadnotes {
    my $i = 1;

    # Look for HNs for all other documents.
    while ($docstr =~ /(<casesum:headnote headnotesource=\"lexis-caselaw-editorial\">)(.*?)(<\/casesum:headnote>)/g) {
	if ($2 =~ /(<text>)(.*?)(<\/text>)/) {
	    my $str = $2;
	    $str =~ s/<.*?>//g;
	    $str =~ s/<\/.*?>//g;
	    $hnote = "$lnistr:HN_$i $str\n\n";
	    print "HEADOUTPUT:" . $hnote;
	    $i++;
	}
    }

    # replace headnote string with a label 'HN_[0-9]+'
    $i = 1;
    while ($docstr =~ /(<casesum:headnote headnotesource=\"lexis-caselaw-editorial\">)(.*?)(<\/casesum:headnote>)/g) {
	$docstr =~ s/(<casesum:headnote headnotesource=\"lexis-caselaw-editorial\">)(.*?)(<\/casesum:headnote>)/HN_$i /;
	$i++;
    }
    
    # ca headnote, format <casesum:headnote>
    while ($docstr =~ /((<casesum:headnote headnotesource=\"ca-official-reporter\">)(.*?)(<ref:anchor id=\"hnpara_([0-9]+)\"\/>)(.*?)(<\/casesum:headnote>))/g) {
	my $num = $5;
	if ($6 =~ /(<text>)(.*?)(<\/text>)/)	{
	    my $str = $2;
	    $str =~ s/<.*?>//g;
	    $str =~ s/<\/.*?>//g;
	    
	    if ( $str =~ / \" / ) {
		$str =~ s/ \" ([^"]+\S)\"/ \"$1\"/g ;
		$str =~ s/ \"(\S[^"]+) \" / \"$1\" /g ;
		$str =~ s/ \" ([^"]+) \" / \"$1\" /g ;
	    }
	    print "HEADOUTPUT:" . "$lnistr:CAL_HN_$num $str\n\n";
	}
    }
    
    # format <casesum:headnote-ref>
    while ($docstr =~ /((<casesum:headnote-ref.*?\">).*?<label>(.*?)<\/label>.*?(<\/casesum:headnote-ref>))/g) {
	$docstr =~ s/((<casesum:headnote-ref.*?\">).*?(<\/casesum:headnote-ref>))/CAL_HN_$3 /;
    }
    return; 
}

# ==================================================
# @brief extract footnotes from document.
# @param $doc, a global variable. 
# @return none. 
# ==================================================
sub getFootnotes {
    my $i = 1; 
    # Look for all the footnotes within the document.
    while ($docstr =~ /(<footnote>)(.*?)(<\/footnote)/g) {
	if ($2 =~ /(<text>)(.*?)(<\/text>)/)	{
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
	    $i++;
	}
    }
    
    $i=1;
    while ($docstr =~ /(<footnote>)(.*?)(<\/footnote>)/g) {
	$docstr =~ s/(<footnote>)(.*?)(<\/footnote>)/FN_$i /;	
	$i++;
    }
    return;
}

# ==================================================
# @brief extract text from the document. 
# @param $docstr, the global document string. 
# @return none. 
# ==================================================
sub getText {
    my $i=1;
    my $parstr = "";
    
    # This part does the actual paragraph extraction.
    while  ($docstr =~ /<p>(.*)<\/p>/g) {
	my $par = $1;
	while ($par =~ /<text>(.*?)<\/text>/g) {
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
		$parstr = $parstr."PARAGRAPH_".$i."\n".$partxt."\n\n";
		$i++;
	    }
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
    return;
}
