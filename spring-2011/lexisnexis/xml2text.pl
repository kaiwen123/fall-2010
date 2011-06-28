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

# Including sentence cutting module. 
BEGIN {
    push @INC, "/data/simon"; 
}

use tosentence ;
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
# %metastr - variable for storing metadata as string; 
# meta string will be of the form as:
# key  <--> value. 
# S_10 <--> State Nat'l Bank v. Farah
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

    # %metastr is used to store metadata so that every time when I do 
    # text extraction and sentence cutting, I can remove the hassle of 
    # dealing with the large number of abbreviations within the string. 
    # And then change back the string that was replaced by a label, 
    # labels in this project include ((S|L|HN|CA_HN|FN|)_[0-9]+).
    %metastr = ();

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
	my $citestr = $5;
	my $actuallni = ""; 
	if ( $timepass =~ /lni=\"([A-Z0-9-]+)\"/) { 
	    $actuallni = $1; 
	    $actuallni =~ s/-//g; 
	}
	$citestr =~ s/<.*?>//g;
	$citestr =~ s/<\/.*?>//g;
	$citation = $lnistr.":L_".$i."::".$id."::".$actuallni."::".$citestr."\n";
	print "CASEREFS:" . $citation;

	my $citeid = "L_$i"; 
	$metastr{$citeid} = $citestr; 
	# print $metastr{$citeid} . "\n"; 
	$docstr =~ s/\Q$string/ $citeid /g; # $citestr/;
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
	my $citestr = $3;
	$citestr =~ s/<.*?>//g;
	$citestr =~ s/<\/.*?>//g;

	my $citeid = "S_$i"; 
	$metastr{$citeid} = $citestr; 
	print "CASEREFS:" . $lnistr.": S_".$i."::$token $citestr\n";
	$docstr =~ s/\Q$string/ S_$i /g; # $citestr/;
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
    my $hnoteid;

    # Look for HNs for all other documents.
    while ($docstr =~ /(<casesum:headnote headnotesource=\"lexis-caselaw-editorial\">(.*?)<\/casesum:headnote>)/g) {
	my $headnotestr = $1; 
	$hnoteid = "HN_$i";
	$docstr =~ s/$headnotestr/ $hnoteid /g; 

	if ($headnotestr =~ /(<text>)(.*?)(<\/text>)/) {
	    my $hnstr = $2;
	    $hnstr =~ s/<.*?>//g;
	    $hnstr =~ s/<\/.*?>//g;
	    $metastr{$hnoteid} = $hnstr;
	    print "HEADOUTPUT:" . "$lnistr:$hnoteid $hnstr\n\n";
	    $i++;
	}
    }

    # ca headnote, format <casesum:headnote>
    while ($docstr =~ /(<casesum:headnote headnotesource=\"ca-official-reporter\">(.*?)(<ref:anchor id=\"hnpara_([0-9]+)\"\/>)(.*?)<\/casesum:headnote>)/g) {
	my $cahnstr = $1;
	my $num = $4;
	$hnoteid = "CA_HN_$num";
	$docstr =~ s/$cahnstr/$hnoteid /;
	$metastr{$hnoteid} = "EMPTY"; 

	if ($cahnstr =~ /(<text>)(.*?)(<\/text>)/)	{
	    my $hnstr = $2;
	    $hnstr =~ s/<.*?>//g;
	    $hnstr =~ s/<\/.*?>//g;
	    
	    if ( $hnstr =~ / \" / ) {
		$hnstr =~ s/ \" ([^"]+\S)\"/ \"$1\"/g ;
		$hnstr =~ s/ \"(\S[^"]+) \" / \"$1\" /g ;
		$hnstr =~ s/ \" ([^"]+) \" / \"$1\" /g ;
	    }
	    print "HEADOUTPUT:" . "$lnistr:$hnoteid $hnstr\n\n";
	}
    }

    return; 
}

# ==================================================
# @brief extract footnotes from document.
# @param $doc, a global variable. 
# @return none. 
# ==================================================
sub getFootnotes {
    my $fnoteid; 
    my $i = 1; 
    # Look for all the footnotes within the document.
    while ($docstr =~ /((<footnote>)(.*?)(<\/footnote))/g) {
	my $fnotestr = $1; 
	$fnoteid = "FN_$i"; 
	$metastr{$fnoteid} = "EMPTY";
	$docstr =~ s/$fnotestr/$fnoteid /g;

	if ($fnotestr =~ /(<text>)(.*?)(<\/text>)/)	{
	    $fnote = $2;
	    $fnote =~ s/<.*?>//g;
	    $fnote =~ s/<\/.*?>//g;
	    $fnote =~ s/  +/ /g;
	    $fnote =~ s/\( +/\(/g;
	    
	    if ( $fnote =~ / \" / ) {
		$fnote =~ s/ \" ([^"]+\S)\"/ \"$1\"/g ;
		$fnote =~ s/ \"(\S[^"]+) \" / \"$1\" /g ;
		$fnote =~ s/ \" ([^"]+) \" / \"$1\" /g ;
	    }
	    $metastr{$fnoteid} = $fnote;
	    # print $metastr{$fnoteid} . "\n";

	    print "FOOTOUTPUT:" . $lnistr."$fnoteid ".$fnote."\n\n";
	    $i++;
	}
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
	# Do sentence cutting here. 
    	print $parstr;
	
    }
    return;
}


# ==================================================
# @brief Cut paragraph into sentence. 
# @param $parstr - the paragraph string to cut.  
# @return sentences that are cutted. 
# ==================================================
sub cut2sentence {
    my $parstr = $_[0]; 
    # Pre-processing paragraph string. 

    # Do sentence cutting to the string. 

    # Post-processing the sentence string. 
}
