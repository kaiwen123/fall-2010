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
# @revision 07/15/2011 readjust comments to reflect changes on code. 
# Move sentence cutting work into a separate module file. 

# Counting the number of documents processed. 
my $cnt = 1;

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
# %metastr - variable for storing metadata as string; 
# meta string will be of the form as:
# key  <--> value. Such as, S_10 <--> State Nat'l Bank v. Farah.
# 
# @comments Due to the large size of the input legal document, it is 
# advisible to pass reference to function by reference rather than by 
# value. And $lnistr is treated as a global variable such that all the 
# functions in the script can use it. 
# Added 07/15/2011. Although pass by reference can be used to pass parameters 
# into functions, it is better to use a global variable for parameter passing
# and eliminate the hassle of handling the function parameters. 
# ======================================================================
# The main loop. 
# ======================================================================
MAINLOOP:while (<STDIN>) {
    # a global string variable. 
    our $docstr = $_; 
    our $lnistr = "";

    s/§/S/g;
    s/ //g;
    s/\$ /\$/g;
    s/\&amp\;/\&/g;

    print "\nProcessing file number " . $cnt . ">>>>>\n";

    # %metastr is used to store metadata so that every time when I do 
    # text extraction and sentence cutting, I can remove the hassle of 
    # dealing with the large number of abbreviations within the string. 
    # And then change back the string that was replaced by a label, 
    # labels in this project include ((|S|L|HN|FN|)_[0-9]+).
    our %metastr = ();

    # ==============================
    # Extract text and meta-data. 
    # ==============================
    # get lni string for the doc. 
    &getLNI();

    # By comparing with the real document, we only process representation 
    # and opinions part. And ignore case summary and consel part, which include 
    # citations and paragraphs. 
    if ($_ =~ /<courtcase:representation>(.*?)<\/courtcase:representation>/){
	$docstr = $1; 
    }
    if ($_ =~ /<courtcase:opinions[^>]*>(.*)<\/courtcase:opinions>/) {
	$docstr .= " " . $1; 
    }

    # Preprocessing of the document, mainly correct the case citation problems
    # 1, if no lni is provided, we need to go over the xml doc and search local
    #    the same local id and resolve the lni. 
    # 2, if no info is given as what type of citation it is, we can analyze the 
    #    context info and get make a decision. 
    &preProcessXml(); 

    # Look for all link citations with lni's involved.
    &getCCitations();
#=begin DEBUG

    # Look for statutory citations.
    &getSCitations();
    
    # Look for headnotes. 
    &getHeadnotes(); 

    # Look for footnodes. 
    &getFootnotes();

    # Get text from the document. 
    &getText();
#=end DEBUG
#=cut
    $cnt++;
}

# ==================================================
# @brief Preprocess the xml document, mainly correct
# errors and unresolved issues. 
# @param the global document will be used. 
# @return none. 
# ==================================================
sub preProcessXml {
    # Eliminate noise and unstandard characters.
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
    while ($docstr =~ /((.{200})(<lnci:cite ID=\"([0-9A-Z]+)\"[^>]*normprotocol=\"lexsee\"[^>]*>(.*?)<\/lnci:cite>))/g) {
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

	$metastr{$citeid} = $citestr; 

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
    	# print "CASEREFS:" . $lnistr . ":$citeid" . "::$destlni:$citestr\n";
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
	#print "CASEREFS:" . "$lnistr:$citeid" . "::$citestr\n";

	$metastr{$citeid} = $citestr; 
	$i++; 
    }

    return; 
}

# ==================================================
# @brief extract headnotes from document.
# @param $docstr, a global variable. 
# @return none. 
# ==================================================
sub getHeadnotes {
    my $i = 1;
    my $hnoteid;

    #print "\nHeadnote:\n";

    # extract headnote tag. 
    while ($docstr =~ /(<casesum:headnote[^>]*>(.*?)<\/casesum:headnote>)/g) {
	my $headnotestr = $1; 

	$hnoteid = "HN_$i";
	$docstr =~ s/\Q$headnotestr/ $hnoteid /g; 

	$headnotestr =~ s/<\/p>/\n/g; 
	$headnotestr =~ s/<p>//g; 

	if ($headnotestr =~ /(<text>)(.*?)(<\/text>)/) {
	    my $hnstr = $2;
	    $hnstr =~ s/<.*?>//g;
	    $hnstr =~ s/<\/.*?>//g;
	    $metastr{$hnoteid} = $hnstr;
	    #print "HEADOUTPUT:" . "$lnistr:$hnoteid:$hnstr\n\n";
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

    #print "\nFootnote:\n";

    # Look for all the footnotes within the document.
    while ($docstr =~ /((<footnote>)(.*?)(<\/footnote>))/g) {
	my $fnotestr = $1; 
	$fnoteid = "FN_$i"; 
	# get footnote id from anchor. 
	# if ($fnotestr =~ m/<ref:anchor id=\"(fn_fnote[0-9]+)\"/) {
	#     $fnoteid=$1; 
	# }
	$metastr{$fnoteid} = "EMPTY";
	$docstr =~ s/\Q$fnotestr/$fnoteid /g;

	if ($fnotestr =~ /(<text>)(.*?)(<\/text>)/)	{
	    $fnote = $2;
	    $fnote =~ s/<[^>]+>//g; 
	    $fnote =~ s/  +/ /g;
	    $fnote =~ s/\( +/\(/g;
	    
	    if ( $fnote =~ / \" / ) {
		$fnote =~ s/ \" ([^"]+\S)\"/ \"$1\"/g ;
		$fnote =~ s/ \"(\S[^"]+) \" / \"$1\" /g ;
		$fnote =~ s/ \" ([^"]+) \" / \"$1\" /g ;
	    }
	    $metastr{$fnoteid} = $fnote;
	    # print $metastr{$fnoteid} . "\n";

	    #print "FOOTOUTPUT:" . "$lnistr:$fnoteid:$fnote\n\n";
	    $i++;
	}
    }
    
    return;
}

# ==================================================
# @brief extract text from the document. 
# NOTE: paragraphs like clspara_[0-9]+ will be ignored here.
# @param $docstr, the global document string. 
# @return none. 
# ==================================================
sub getText {
    my $parstr = ""; 
    my $cnt = 1; 
    my $segmentstr = "ZZZZZ"; 

    print "\n\n" . $lnistr . "\n\n"; # identifier of the document.

    while ($docstr =~ m/(<blockquote[^>]*>(.*?)<\/blockquote[^>]*>)/g) {
	my $quotestr = $1;	# quoted string with xml tag. 
	my $quotepar = $2; 	# quoted paragraph. 
	
	$quotepar =~ s/<[^>]+>/ /g;
	# print $parid . "  " . $parstr . "\n\n\n"; 
	$docstr =~ s/\Q$quotestr/ $segmentstr $quotepar $segmentstr /g;
    }

    # extract all the paragraph data. 
    while($docstr =~ m/(<p>(.*?)<\/p>|<h>(.*?)<\/h>)/g) {
	$parstr = $1; 

	# paragraphs embedded within paragraph can be identified by
	# <\/?blockquote>.
	# $parstr =~ s/<emph typestyle="bf">(.*?)<\/emph>/$segmentstr $1 $segmentstr/g; 
	$parstr =~ s/<mock-para-break[^>]*>/$segmentstr/g;
	$parstr =~ s/<[^>]*>/ /g;
	# print $parstr . "\n\n";

	# adjust the paragraphs, some can not be a seperate paragraph. 
	while ($parstr =~ m/($segmentstr( +[a-z]\w*[^\.] ))/g) {
	    my $segcontext = $1; 
	    my $segremoved = $2; 
	    $parstr =~ s/$segcontext/$segremoved/g; 
	}
	my @pars = split(/$segmentstr/, $parstr); 

	for $_ (@pars) {
	    next if (m/^ *$/); 

	    s/  +/ /g; 
	    s/^ +//g; 
	    s/(\w) +([\.,])/$1$2/g; # remove space before [\.\,]. 
	    s/ +\)/\)/g;
	    s/…/\.\.\./g;    # remove the special characters. 

	    print "\nPARAGRAPH_" . $cnt . "\n" . $_ . "\n";

	    #&preProcess(\$_); 
	    #&postProcess(\$_); 
	    $cnt++;
	}
    }
    return ;
}
