#!/usr/bin/perl -w
# @file xml2text.pl. 
# @brief convert the xml input file into text format, in this process the
# footnotes, headnotes and the citation metadata will be extracted.
# @func getLNI(), getLCitations(), getSCitations(), extractText(). 
# @author Vinayak & Simon Guo. 
# @revision 04/10/2011 created by Vinayak. 
# @revision 06/16/2011 updated comments by Simon Guo. 
# @revision 06/23/2011 added sentence cutting part by Simon Guo. 
# Move sentence cutting work into a separate module file. 
# @revision 06/27/2011 added detailed comments. 
# @revision 07/15/2011 re-adjust comments to reflect changes on code. 
# @revision 08/17/2011 redo the paragraph cutting work to include the missing output. 
# @revision 08/21/2011 added summaries part into the paragraph output if 
# there are any citations.
# @revision 08/24/2011 added representation into a seperate file. 
# @revision 08/24/2011 remove the %metastr hash variable, it is not useful. 

# ======================================================================
# @brief this is the main loop over all the legal documents. It will go 
# over all input legal issue documents(usually, each line is a legal doc)
# and extract footnotes, headnotes, citation metadata and paragraph text.
# 
# Due to the large size of input xml data, we need to keep in mind that 
# data processing efficiency is an important consideration. For example, 
# we need to consider function parameter passing method, how to handle 
# output and the time complexity of the implementation. 
# 
# Distributed computing has become an important aid to processing large 
# volumes of data, so in order to use the power of distributed computing 
# such as hadoop, I used <STDIN> as input to the program and <STDOUT> as 
# output file. And for ease of identification of each meta-data and text
# data, I prependded special marks for each type of data so that we can 
# use filtering to separate these texts in to different files. 
# 
# @input <STDIN>, each line of input is an issue document in xml format. 
# @output <STDOUT>, the meta-data we need include footnote, headnote, 
# citation meta-data and paragraph/sentences. 
# 
# @var Global variables : 
# $lnistr - the doc unique doc as string; 
# $docstr - courtcase representation string; 
# 
# @comments Due to the large size of the input legal document, it is 
# advisible to pass reference to function by reference rather than by 
# value. And $lnistr is treated as a global variable such that all the 
# functions in the script can use it. 
# Added 07/15/2011. Although pass by reference can be used to pass parameters 
# into functions, it is better to use a global variable for parameter passing
# and eliminate the hassle of handling the function parameters. 
# Counting the number of documents processed. 
my $doccnt = 1;
# ======================================================================
# The main loop. 
# ======================================================================
MAINLOOP:while (<STDIN>) {
    # a global string variable. 
    our $docstr = $_; 
    our $lnistr = "";
    our $segmentstr = "ZZZZZ"; 	# for segmentation of paragraphs. 

    print "\nProcessing file number " . $doccnt . ">>>>>\n";

    # ==============================
    # Extract text and meta-data. 
    # ==============================
    # get lni string for the doc. 
    &getLNI();

    # By comparing with the real document, we only process representation 
    # and opinions part. And ignore case summary and consel part, which include 
    # citations and paragraphs. 
    $docstr = "";
    if (m/(<casesum:summaries[^>]*>.*?<\/casesum:summaries[^>]*>)/) {
	$docstr .= $1; 
    }
    if (m/(<courtcase:representation[^>]*>.*?<\/courtcase:representation[^>]*>)/){
	$docstr .= $1; 
    }
    if (m/(<courtcase:opinions[^>]*>(.*)<\/courtcase:opinions[^>]*>)/) {
	$docstr .= $1; 
    }

    # Preprocessing of the document, mainly correct the case citation problems
    # 1, if no lni is provided, we need to go over the xml doc and search local
    #    the same local id and resolve the lni. 
    # 2, if no info is given as what type of citation it is, we can analyze the 
    #    context info and get make a decision. 
    &preProcessXml(); 

    # Look for all link citations with lni's involved.
    &getCCitations();

    # Look for statutory citations.
    &getSCitations();
    
    # Look for headnotes. 
    &getHeadnotes(); 

    # Look for footnodes. 
    &getFootnotes();

    # last MAINLOOP;
    # Get summaries part of the document. 
    &getSummaries(); 

    # Get representations part of the document. 
    &getRepresentation(); 

    # Get text from the document. 
    &getOpinion();

    $doccnt++;
}

# ==================================================
# @brief Preprocess the xml document, mainly correct
# errors and unresolved issues. 
# @param the global document will be used. 
# @return none. 
# ==================================================
sub preProcessXml {
    # Eliminate noise and unstandard characters.
    # $docstr =~ s/§/S/g;
    $docstr =~ s/ //g;
    $docstr =~ s/\$ /\$/g;
    $docstr =~ s/\&amp\;/\&/g;		# transform ascii chars to normal form. 

    # correct the unresolved lni citation. 

}

# ==================================================
# @brief Get the LNI string from the document. This value is constant
# throughout the document. 
# @param global variable $docstr will be used.
# @return The document LNI string. 
# ==================================================
sub getLNI {
    if ($docstr =~ '<lncr:persistentidentifier>(.*?)<\/lncr:persistentidentifier>')
    {
	$lnistr = $1;
	$lnistr =~ s/\-//g;
    }
    return;
}

=begin citation
    Determinination of case citation is dependent on the normprotocol property
    of the citation, if normprotocol="lexsee", citation is a case citation, else
    if normprotocol ="lexstat", citation is a statutory citation. But there are
    errors within the input xml files, such as what should we do if there is no
    normprotocol property, how do we handle it? 
=end citation
=cut 


# ==================================================
# @brief This routine helps in finding the Case citations. That is "A vs. B"
# kind of citations. This captures and records casereftokens in the
# file. And the format it captures is as follows:
# Lni-current document::CC_(citation_number)::TokenID::LNI-target document:: \
# Actual citation 
# @param $docstr  which are global variables. 
# @param lnistr which is the global variable.
# @return case citation strings in the input string.
# ==================================================
sub getCCitations {
    my $i = 1; 
    my $citeid; 
    my $citestr; 
    my $citecontent;
    my $localciteid; 
    my $destlni; 

    # print "\nCase Citations\n";
    # need to verify the structure of original xml file. 
    while ($docstr =~ /((.{200})?(<lnci:cite ID=\"([0-9A-Z]+)\"[^>]*normprotocol=\"lexsee\"[^>]*>(.*?)<\/lnci:cite>))/g) {
	$destlni = $2;
	$citestr = $3; 
	$localciteid = $4; 

	$citeid = "CC_$i";

	my $replstr = $citestr; # used to handle unbalanced braces. 

	# some citation doesn't contain destination lni string. 
	if ($destlni =~ m/lni=\"([A-Z0-9\-]+)\"/g) {
	    $destlni = $1; 
	    $destlni =~ s/-//g;
	} else {
	    $destlni = "UNRESOLVED";
	}

    	# get cite content. 
	$citestr =~ s/<[^>]+>/ /g;
	$citestr =~ s/  +/ /g;
	$citestr =~ s/^ +//g;
	$citestr =~ s/  +([,])/,/g; # remove space in front of comma etc. 

	# handle unbalanced brakets. 
	if($citestr =~ m/\)\)/g) {
	    $docstr =~ s/\Q$replstr/ $citeid) /g;
	    $citestr =~ s/\)\)/\)/g;
	} elsif ($citestr =~ m/^[^\(\)]*\)\W*$/g) {
	    $docstr =~ s/\Q$replstr/ $citeid) /g;
	    $citestr =~ s/\)//g;
	} else {
	    $docstr =~ s/\Q$replstr/ $citeid /; 
	}

    	# vinayak print "CASEREFS:$citeid:\t" . $lnistr . ":$localciteid" . "::$destlni:$citestr\n";
    	print $lnistr . ":" . $citeid . "::" . $destlni . ":" . $citestr . "\n";
	$i++;
    }

    return;
}

# ==================================================
# @brief As compared with the above routine, this routine helps in finding
# statutory citations in the document. The format it captures is as follows:
# Lni-current document::S_(citation_number)::localID::citation string. 
# When extracting statutory citaions, we need to consider cases that might 
# contain citations. Currently, I have identified three cases. 
# case 1: <lnci:cite ... normprotocol="lexstat"; 
# case 2: <ref:cite4thisresource...>; 
# case 3: <lnci:cite ...>. which does not contain the normprotocol property. 
# @param $docstr which are global variables for each doc. 
# @param lnistr - the document unique string. 
# @return none, but will update the global $docstr. 
# ==================================================
sub getSCitations {
    my $i = 1; 
    my $localid = ""; 
    my $citestr; 
    my $citeid;

    #print "\nStatutory Citations\n";

    # Process citations in head of doc. 
    # xml node is: <ref:cite4thisresource...>
    # added by simon on July 05, 2011. 
    while ($docstr =~ m/((<lnci:cite ID=\"([A-Z0-9]+)\"[^>]*normprotocol=\"lexstat\"[^>]*>)(.*?)<\/lnci:cite>)/g) {
	$citestr = $1;
	$localid = $3; 
	my $citetype = $4; 
	if (! $localid) {
	    $localid = "UNDETERMINED";
	}

	$citeid = "SC_$i";
	my $replstr = $citestr; # handle unbalanced braces. 

	$citestr =~ s/<[^>]+>/ /g; 
	$citestr =~ s/  +/ /g; 
	$citestr =~ s/^ +//g;

	# handle unbalanced brakets. 
	if($citestr =~ m/\)\)/g) {
	    $docstr =~ s/\Q$replstr/ $citeid) /g;
	    $citestr =~ s/\)\)/\)/g;
	} elsif ($citestr =~ m/^[^\(\)]*\)\W*$/g) {
	    $docstr =~ s/\Q$replstr/ $citeid) /g;
	    $citestr =~ s/\)//g;
	} else {
	    $docstr =~ s/\Q$replstr/ $citeid /; 
	}

	# vinayak print "CASEREFS:$citeid:\t" . "$lnistr:$localid" . "::$citestr\n";
	print $lnistr . ":" . $citeid . "::" . $citestr . "\n";

	$i++; 
    }

    return; 
}

# ==================================================
# @brief extract headnotes from document.
# Note: <casesum:headnote-grp> is located within the header 
# of the document, so it will be ignored. Changes can be 
# made to bring this back when necessary. 
# @param $docstr, a global variable. 
# @return none. 
# ==================================================
sub getHeadnotes {
    my $i = 1;
    my $hnoteid;

    #print "\nHeadnote:\n";

    # extract headnote tag. 
    # we need to ignore the headnote-grp within the summaries part. 
    while ($docstr =~ /(<casesum:headnote [^>]*>.*?<\/casesum:headnote[^>\-]*>)/g) {
	my $headnotestr = $1; 

	$hnoteid = "HN_$i";
	$docstr =~ s/\Q$headnotestr/ $hnoteid /g; 

	if ($headnotestr =~ /(<text>(.*?)<\/text>)/) {
	    my $hnstr = $2;
	    $hnstr =~ s/<[^>]*>//g;
	    $hnstr =~ s/  +/ /g;
	    print $lnistr . ":" . $hnoteid . "::" . $hnstr . "\n";
	    $i++;
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
    while ($docstr =~ /(<footnote>(.*?)<\/footnote>)/g) {
	my $fnotestr = $1; 
	$fnoteid = "FN_$i"; 
	$docstr =~ s/\Q$fnotestr/$fnoteid /g;

	while ($fnotestr =~ /(<text>(.*?)<\/text>)/) {
	    $_ = $2; 
	    $fnotestr =~ s/\Q$1//g;
	    s/<[^>]+>//g; 
	    s/  +/ /g;
	    s/\( +/\(/g;	# remove space after ( and in front of ).
	    s/ +\)/\)/g;
	    print $lnistr. ":" . $fnoteid . "::" . $_ . "\n";
	}
	$i++;
    }
    return;
}

# **************************************************
# NOTES: summary and representation are text data 
# that contains citations, but as of the current implementation. 
# we only need to consider the work of the judicial opinion
# part, so we output the summaries and representations 
# into seperate files.
# **************************************************
# 
# ==================================================
# @brief extract text from the document. 
# NOTE: paragraphs like clspara_[0-9]+ will be ignored here.
# @param $docstr, the global document string. 
# @return none. 
# ==================================================
sub getSummaries {
    # get summary part of the text. 
    if ($docstr =~ /(<casesum:summaries[^>]*>.*?<\/casesum:summaries[^>]*>)/) {
	$_ = $1; 
	$docstr =~ s/\Q$_/ /g;
	# if summary does not contain citations, ignore it; or else get it.
	if (/[CS]C_[0-9]+/) {
	    s/(<\/text[^>]*>)/$1$segmentstr/g;
	    $part = "SUMMARY";
	    &xml2par($_);
	}
    }
}

# ==================================================
# @brief extract text from representation part of the input. 
# @param $docstr, the global document string. 
# @return none. 
# ==================================================
sub getRepresentation {
    if ($docstr =~ /(<courtcase:representation[^>]*>.*?<\/courtcase:representation[^>]*>)/) {
	$_ = $1; 
	$docstr =~ s/\Q$_/ /g;
	if (/[CS]C_[0-9]+/) {
	    s/(<\/courtcase:counsel[^>]*>)/$1$segmentstr/g;
	    $part = "REPRESENTATION";
	    &xml2par($_);
	}
    }
}

# ==================================================
# @brief extract text from judicial opinions. This part is the 
# main source of data for building the case citation library. 
# @param $docstr, 
# @return none. 
# ==================================================
sub getOpinion {
    my $parstr = ""; 
    my $cnt = 1; 

    $_ = $docstr; 

    # removed redundant text between opinion and body text, will break structure of xml. 
    s/<courtcase:opinion[^>]*>.*?<bodytext>//g; 

    # use </text>, </h> and </courtcase:counsel> to split paragraph. 
    s/(<\/text[^>]*>|<\/h[^>]*>|<\/courtcase:counsel[^>]*>)/$1$segmentstr/g;

    # Segment the block quote pattern. 
    s/(<emph typestyle=\"bf\">[iIvVxX]+\.[^<]*<\/emph>)/$1$segmentstr/g;

    # The first emphasized line should be a title line/ paragraph. 
    s/((<p><ref:anchor[^>]*><text>|<mock-para-break[^>]*>)<emph typestyle=\"bf\">[^<]*<\/emph>)/$1$segmentstr/g;

    # remove the segmentation mark between adjacent emph nodes. 
    s/$segmentstr( *<emph)/$1/g;

    # remove segmentation mark between breaking quotations. 
    s/<\/text[^>]*>( *$segmentstr *)+(<[^>]*>)*([a-z]\w* )/ $3/g;

    $part = "OPINION";
    &xml2par($_);
}

# ==================================================
# @brief split xml text into paragraphs using split mark. 
# @input marked xml text. 
# @output splitted paragraphs.
# ==================================================
sub xml2par {
    # remove xml nodes. 
    $_ = $_[0]; 

    s/<[^>]*>/ /g;		# remove xml tags. 
    s/  +/ /g; 

    my $xmlstr = $_; 
    my @pars = split(/$segmentstr/, $xmlstr); 
    my $parcnt = 1; 		# paragraph count.
    for $_ (@pars) {
	next if (m/^ *$/); 
	s/^ +//g; 
	s/(\w) +([\.,])/$1$2/g; # remove space before [\.\,]. 
	s/ +\)/\)/g;
	s/…/\.\.\./g;    # remove the special characters. 

	print $lnistr . ":" . $part . "_PARAGRAPH_" . $parcnt . "::" . $_ . "\n";

	$parcnt++;
    }
    return ;
}
