#!/usr/bin/perl -w
# @brief This script implements cutting of paragraphs 
# into sentences. This module file will be used by 
# the xml2text.pl file. 
# @revision 07/15/2011 initial version by Simon.
# @revision 08/01/2011 Added code to handle braces. 
# @revision 08/22/2011 

use Lingua::EN::Sentence qw( get_sentences add_acronyms );

# main loop 
# ignore special paragraphs and process the rest of the paras. 
&loadAbbrev("abbrev.abb");	# Load only once. 
LOOP:while (<STDIN>) {
    our $parstr = "";		# paragraph data. 
    our $parid = ""; 		# paragraph id. 

    # obtain paragraph id first. 
    if (m/([0-9A-Z]+:[A-Z]+_PARAGRAPH_[0-9]+::)/g) {
	$parid = $1; 
	s/\Q$parid//g; 
	$parid =~ s/:://g; 
	print "\n" . $parid . "\n";
	$parstr = $_;
    } else { next; }

    &preProcess(); 
    &postProcess(); 
}
 
# ==================================================
# @brief This process will be used to remove all the
# errors that can cause the failure of the sentence 
# cutting program. 
# @param $parstr The paragraph string. 
# @return None.
# ==================================================
sub preProcess {
    $replstr = "ZZZZZ";		# for replacing dot(\.).
    $replstr1 = "XXXXX"; # for replacing ' in pattern like Poor's." Count ...
    $_ = $parstr;
    %bracepart = ();	# store the bracketed sentence. 
    my $cnt = 0;	# counted key for above work. 

    if (m/^ *$/) { return; }
    chomp;

    # remove space before the enclosing symbols(e.g. \(\{\[ etc.). 
    s/([\(]) *([^\)]*[^ ]) *([\)])/$1$2$3/g;
    s/O?\&[lg]t;O?//g;		# delete special characters. 
    s/&apos;/'/g;

    # Remove quotations in paragraph. 
    # &removeQuotes; 
    # add missing space between sentences. 
    s/([A-Za-z]+ \w+[a-z]+\.)([A-Z]\w+[a-z]+)/$1 $2/g; 
    s/ ([,\?\.])/$1/g;		      # remove space in front of [,?].

    # add space in front of numbering. 
    s/[\.]([IVX]+\. )/\. $1/g;

    # if dot for the end of the sentence is replaced, bring it back.
    s/$replstr( [A-Z][a-z]+ )/\.$1/g;
    #s/$replstr( [A-Z]+) /\.$1 /g;

    s/( [a-zA-Z]+)\.([a-zA-Z]+)\. /$1$replstr$2$replstr /g; # e.g: p.m.
    s/(([A-Z]+\.){2,})\.+/$1/g;

    # special cases. 
    s/(\bNo)\.( [A-Z0-9]+\b)/$1$replstr$2/g;
    s/( So)\.( [0-9])/$1$replstr$2/g;
    s/(^P[0-9]+)\. /$1$replstr /g;

    s/(( |^)[iI]d)\./$1$replstr/g; # can not be handled by acronyms because of its location.

    s/(\w+)\'(\w*\.\" [A-Z]\w+)/$1$replstr1$2/g;

    s/(\b\w+)\.( at )/$1$replstr$2/g;

    s/(\w+)\.( \W\"[A-Z]+\"\W)/$1$replstr$2/g; # e.g: Fla. ("MCC") ...

    # Ajust ids. 
    s/([SC]C_[0-9]+[\.\,\;\!\'\"])(\w+)/$1 $2/g; # e.g: S_10,Abc => S_10, Abc

    # adjust numbers. 
    s/((^|[\.\?] |")[0-9]+)\. /$1$replstr /g; # numbering of lists. 
    s/([Nn][oO]\. [0-9]+)$replstr( [A-Z])/$1\.$2/g; # correct error. (e.g. No. 120. The ...)
    s/(^[a-zA-Z])\. /$1$replstr /g;  # alphabetical numbering.
    s/(^[IVXivx]+)\. /$1$replstr /g; # replace the I. II. ... numbering. 
    s/(\. [IVXivx]+)\.( [A-Z]\w+ )/$1$replstr$2/g; # numbering within paragraph. 

    # special cases.
    s/per cent\.( ?[^A-Z])/percent$1/g;	# change "per cent\." to "percent" for non-sentence.
    s/per cent\./percent\./g; # change "per cent\." to "percent" for else.
    s/( etc\.)( [A-Z]\w*)/$1\.$2/g; # when "etc." ends a sentence, add additional dot.
    s/( Amendment)\.( [0-9])/$1$2/g; # Amendment ajustment. 
    s/(\'\"|\"\')/\"/g;		    # change double quote to single quote. 

    s/([\.\?]\"\))( [A-Z]\w+)/$1\.$2/g; # add period after \.\"\) and before Xxxx, ...

    # print $_ . "\n\n";

    # book keep braced sentences. 
    while (m/[\.\"\)]?( |^)([\(\[\{][A-Z]([^\)\]\}]|([\(\[\{][^\)\]\}]*[\)\]\}]))*\.[\)\]\}])([ \"\.])/g) {
	my $s = $2; 
        # ignore those with only one word, and those longer than 100.
	if (($s =~ m/ /g) && (length($s) <= 100)) { 
	    my $key = "BST_".$cnt;
	    $bracepart{$key} = $s;
	    s/\Q$s/ $key. /g; 
	    $cnt++;
	}
    }

    # book keep braced non-sentences. --verify (\.?)
    while (m/([\(\[\{]([^\)\]\]]|([\(\[\{][^\)\]\}]*[\)\]\}]))*[\)\]\}])(\.?)/g){
	my $p = $1; 
	my $dot = $4;
        # consider those with more than two words and longer than 100. 
	if (($p =~ m/ /g) && (length($p) <= 100)) {
	    my $key = "BST_".$cnt;
	    $bracepart{$key} = $p; 
	    s/\Q$p/$key$dot /g;
	    $cnt++; 
	}
    }

    # case anme with lexis string. 
    while (m/([0-9]+[\,\. ](\w+\. )+LEXIS [0-9]+)([\.]?)[,] ?/g){
	my $lexisid = $1;
	my $dot = $3;
	my $key = "BST_" . $cnt; 
	$bracepart{$key} = $lexisid;
	s/\Q$lexisid/$key$dot/g;
	$cnt++; 
    }

    # handling abbrevations. 
    # eg. permit on March, 7, 1990. Plf. Summ. J. Ex. 58. In its review ...
    while (m/\b([A-Za-z]{1,4}\. ?([A-Za-z0-9]{1,4}\. ?){2,})(\w+\.( [A-Z]\w+|$) ?|\w+\W)?/g) {
    	my $origstr = $1; 	# original string;
    	my $transtr = $origstr; # transformed string;

    	$transtr =~ s/\./$replstr/g;
    	s/\Q$origstr/$transtr/g;
    }

    #print $_ . "\n\n";

    $parstr = $_;

    return; 
}

# ==================================================
# @brief sentence cutting using perl sentence module. 
# @param none, actually it will use the package use 
# the global variable called $parstr. 
# @return none. 
# ==================================================
sub postProcess {
    my $cnt = 1; 
    my $s1 = "";
    # &loadAbbrev($file); 
    #print $parstr . "\n"; 
    
    my $sentences = get_sentences($parstr); 
    LOOP:foreach $_ (@$sentences) {
	# replace back the blocks. 
	while (m/((BST_[0-9]+)\.?)/g) {
	    my $replace = $1; 
	    my $key = $2; 
	    # s/\Q$replace/$bracepart{$key}/;
	    s/\Q$key/$bracepart{$key}/;
	}
	# remove unbalanced quotations. 
	s/(^[^\"]*)\"([^\"]*$)/$1 $2/g;	# only one quotation.

	# remove unbalanced braces. 
	s/(^.*)\(([^\)]*$)/$1 $2/g; 
	s/(^[^\(]*)\)(.*$)/$1 $2/g;

	# post-process the sentence.
	s/\.\.\./ /g;		# remove ellipses.
	s/^ +//g;		# leading space removed. 
	s/\s\s+/ /g;	# remove redundant spaces. 
	s/ ,/,/g;	# remove space before ,. 
	s/^\"([^\"]+)/$1/g;	# remove unbalanced quotation. 
	s/$replstr/\./g;
	s/$replstr1/\'/g;
	
	# if there is over split, handle it by splicing the sentence to the previous one. 
	if ($s1 =~ /^$/) {
	    $s1 = $_;
	    next LOOP;
	}

	if (/^[a-z]\w+ /) {
	    my $s = $s1 . " " . $_; 
	    print "$cnt:  " . $s . "\n"; 
	    $s1 = "";
	} else {
	    print "$cnt:  " . $s1 . "\n"; 
	    $s1 = $_;
	}

	$cnt++;
    }

    # not empty, then print it out. 
    if ($s1 !~ /^$/) {
	print "$cnt:  " . $s1 . "\n"; 
    }

    %bracepart = ();
    return; 
}

# 
# Deprecated functions after this line. 
# 
# ==================================================
# @brief load abbrevations from file, and add all the 
# acronyms into EN::Sentence library for handling 
# sentence endings. This is mainly used to disambiguite
# the ending of sentence by period(.). 
# The problem of doing this is that it will be very 
# time consuming to load and check all the input text 
# over all the loaded abbrevations. 
# The format of the abbrevation file is "cnt abbrev.". 
# @param $abbfile, the name of the abbrevation file. 
# @return none, the abbrevations will be loaded into 
# files. 
# ==================================================
sub loadAbbrev {
    my $cnt = 0; 
    my $abbfile = $_[0];
    my @abbv = ();
    open(ABBFILE, "$abbfile") || die("can't open file $abbfile");

    LOOP:while (<ABBFILE>) {
	chomp; 
	@abbv = ();
	@abbv = split(/ /, $_);
	add_acronyms(@abbv);
    }
    close ABBFILE; 
    return; 
}
