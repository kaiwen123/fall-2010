#!/usr/bin/perl
# @brief This script implements cutting of paragraphs 
# into sentences. This module file will be used by 
# the xml2text.pl file. 
# @revision 07/15/2011 initial version by Simon.

use Lingua::EN::Sentence qw( get_sentences add_acronyms );
 
# ==================================================
# @brief This process will be used to remove all the
# errors that can cause the failure of the sentence 
# cutting program. 
# @param $parstr The paragraph string. 
# @return None.
# ==================================================
$replstr = "ZZZZZ";		# for replacing dot(\.).
$replstr1 = "XXXXX"; # for replacing ' in pattern like Poor's." Count ...
sub preProcess {
    $_ = ${$_[0]};
    %bracepart = ();	# store the bracketed sentence. 
    my $cnt = 0;	# counted key for above work. 

    if (m/^ *$/) { return; }
    chomp;

    # remove space before the enclosing symbols(e.g. \(\{\[ etc.). 
    s/([\(]) *([^\)]*[^ ]) *([\)])/$1$2$3/g;
    s/O?\&[lg]t;O?//g;		# delete special characters. 

    # book keep braced sentences. 
    while (m/\.[\"\)]? ([\(\[\{][A-Z]([^\)\]\}]|([\(\[\{][^\)\]\}]*[\)\]\}]))*\.[\)\]\}])([ \"\.])/g) {
	my $s = $1; 
	if ($s =~ m/ /g) {
	    my $key = "BST_".$cnt;
	    $bracepart{$key} = $s;
	    s/\Q$s/ $key. /g; 
	    $cnt++;
	}
    }

    # book keep braced non-sentences. --verify (\.?)
    while (m/([\(\[\{]([^\)\]\]]|([\(\[\{][^\)\]\}]*[\)\]\}]))*[\)\]\}])(\.?)/g){
	my $p = $1; 
	if ($p =~ m/ /g) {	# more than two words. 
	    my $dot = $4;
	    my $key = "BST_".$cnt;
	    $bracepart{$key} = $p; 
	    s/\Q$p/$key$dot /g;
	    $cnt++; 
	}
    }

    while (m/([0-9]+[\,\. ](\w+\. )+LEXIS [0-9]+)([\.]) ?/g){
	my $lexisid = $1;
	my $dot = $3;
	my $key = "BST_" . $cnt; 
	$bracepart{$key} = $lexisid;
	s/\Q$lexisid/$key$dot/g;
	$cnt++; 
    }
    # Remove quotations in paragraph. 
    # &removeQuotes; 
    # add missing space between sentences. 
    s/([A-Za-z]+ \w+[a-z]+\.)([A-Z]\w+[a-z]+)/$1 $2/g; 
    s/ ([,\?\.])/$1/g;		      # remove space in front of [,?].

    # add space in front of numbering. 
    s/[\.]([IVX]+\. )/\. $1/g;

    # handling abbrevations. 
    # eg. permit on March, 7, 1990. Plf. Summ. J. Ex. 58. In its review ...
    while (m/\b([A-Za-z]{1,4}\. ?([A-Za-z0-9]{1,4}\. ?){2,})(\w+\.( [A-Z]\w+|$) ?|\w+\W)?/g) {
    	my $origstr = $1; 	# original string;
    	my $transtr = $origstr; # transformed string;

    	$transtr =~ s/\./$replstr/g;
    	s/\Q$origstr/$transtr/g;
    }

    # if dot for the end of the sentence is replaced, bring it back.
    s/$replstr( [A-Z][a-z]+ )/\.$1/g;
    #s/$replstr( [A-Z]+) /\.$1 /g;

    s/( [a-zA-Z]+)\.([a-zA-Z]+)\. /$1$replstr$2$replstr /g; # e.g: p.m.
    s/(([A-Z]+\.){2,})\.+/$1/g;

    # special cases. 
    s/(\bNo)\.( [A-Z0-9]+\b)/$1$replstr$2/g;
    s/( So)\.( [0-9])/$1$replstr$2/g;
    s/(^P[0-9]+)\. /$1$replstr /g;

    # ==================================================
    # Add acronyms.
    # ==================================================
    add_acronyms('art', 'ab', 'tit', 'pen', 'supp', 'bhd', 'indus', 'civ','j','affd'); 
    add_acronyms('seq', 'cert', 'disc', 'etc', 'cf', 'ed', 'ch', 'fed', 'cir','sec');
    add_acronyms('cong', 'sess', 'admin', 'ann', 'stat', 'cr','am','pet','mem','br');
    add_acronyms('nos','commn','syl','constr');

    # old style state acronyms. 
    add_acronyms('Ala','Ark','Ariz','Calif','Colo','Conn','Del','fla','Ga','Ill'); 
    add_acronyms('Ind','Kan','Ky','La','Mass','Md','Mich','Minn','Mo.','Miss','Mont','Neb'); 
    add_acronyms('Nev','Okla','Ore','Pa','Tenn','Vt','Va','Wash','Wis','Wyo');

    s/(( |^)[iI]d)\./$1$replstr/g; # can not be handled by acronyms because of its location.

    s/(\w+)\'(\w*\.\" [A-Z]\w+)/$1$replstr1$2/g;

    s/(\b\w+)\.( at )/$1$replstr$2/g;

    s/(\w+)\.( \W\"[A-Z]+\"\W)/$1$replstr$2/g; # e.g: Fla. ("MCC") ...

    # Ajust ids. 
    s/([SC]C_[0-9]+[\.\,\;\!\'\"])(\w+)/$1 $2/g; # e.g: S_10,Abc => S_10, Abc

    # adjust marks. like .... ... etc. 
    s/\.\.\.\.?( [^A-Z])/, $1/g; # e.g: .... by => , by
    s/\.\.\.\.?( [A-Z])/\. $1/g; # e.g: .... By => . By
    s/\.\.\.?\.?([^ ])/\.$1/g;	 # e.g: ..., by => ., by

    # adjust numbers. 
    s/((^|[\.\?] |")[0-9]+)\. /$1$replstr /g; # numbering of lists. 
    s/([Nn][oO]\. [0-9]+)$replstr( [A-Z])/$1\.$2/g; # correct error. (e.g. No. 120. The ...)
    s/(^[a-zA-Z])\. /$1$replstr /g;  # alphabetical numbering.
    s/(^[IVX]+)\. /$1$replstr /g; # replace the I. II. ... numbering. 
    s/(\. [IVX]+)\.( [A-Z]\w+ )/$1$replstr$2/g; # numbering within paragraph. 

    # special cases.
    s/per cent\.( ?[^A-Z])/percent$1/g;	# change "per cent\." to "percent" for non-sentence.
    s/per cent\./percent\./g; # change "per cent\." to "percent" for else.
    s/( etc\.)( [A-Z]\w*)/$1\.$2/g; # when "etc." ends a sentence, add additional dot.
    s/(\'\"|\"\')/\"/g;		    # change double quote to single quote. 

    s/(\w+\.\"\))( [A-Z]\w+)/$1\.$2/g; # add period after \w+\.\"\) and before Xxxx, ...

    print $_ . "\n\n";

    ${$_[0]} = $_;

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
    my $file = "abbrev.abb";
    # &loadAbbrev($file); 
    my $sentences = get_sentences(${$_[0]}); 
    foreach $sentence (@$sentences) {
	$_ = $sentence; 
	while (m/((BST_[0-9]+)\.?)/g) {
	    my $replace = $1; 
	    my $key = $2; 
	    s/\Q$replace/$bracepart{$key}/;
	}
	# post-process the sentence.
	s/\s\s+/ /g;	# remove redundant spaces. 
	s/ ,/,/g;	# remove space before ,. 
	s/^\"([^\"]+)/$1/g;	# remove unbalanced quotation. 
	s/$replstr/\./g;
	s/$replstr1/\'/g;
	print "$cnt:  " . $_ . "\n"; 
	$cnt++;
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
    open(ABBFILE, "$abbfile");

    LOOP:while (<ABBFILE>) {
	chomp; 
	s/[0-9]+ +//g; 		# remove counting.
	# print $_ . " ";
	push @abbv, "$_"; 
	$cnt++;
	if ($cnt >= 600) {
	    last LOOP;
	}
    }
    add_acronyms(@abbv);

    close ABBFILE; 
    return; 
}

# =======================================================================
# initially obtained from Paul's previous work. This routine is to be
# adjusted according to the requirement of current sentence cutting work. 
# Remove " at beginning and end position of a paragraph if proper
# Remove long quoted string in the middle if proper
# Input Paragraph is in $_
# =======================================================================
sub removeQuotes {}

1; 
