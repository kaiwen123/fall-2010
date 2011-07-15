#!/usr/bin/perl
# @brief This script implements cutting of paragraphs into sentences. 
# @revision 07/15/2011 initial version by Simon.
# 
#

use Lingua::EN::Sentence qw( get_sentences add_acronyms );
 
# ==================================================
# @brief This process will be used to remove all the
# errors that can cause the failure of the sentence 
# cutting program. 
# @param $parstr The paragraph string. 
# @return None.
# ==================================================
sub preProcess {
    $parstr = $_[0];
    chomp;
    # ==================================================
    # Correct errors and abnormal forms of the input. 
    # ==================================================
    # rule R_0; handling citation case names, -- casename.pl 
    # rule example: in file -> casename.example. 

    # rule R_1; abbrevation handling. -- abbrev.pl 
    # rule example: in file -> abbrev.example. 
    s/([0-9a-z]+\."?)([A-Z])/$1 $2/g; # Add space between sentences. 

    # rule R_2; labels handling. -- label.pl
    # rule example: in file -> label.example.
    s/(\w+)\. (((FN|HN|L|S)_[0-9]+ )+)([A-Z])/$1, $2\. $5/g; # adjust "xxx. FN_10 Foo "
    s/([\.\?]\") (((FN|HN|L|S)_[0-9]+ )+)([A-Z])/$1, $2\. $5/g; # adjust "xxx." FN_10 Foo "

    s/ ([,\?\.])/$1/g;		      # remove space in front of [,?].

    # ==================================================
    # Add acronyms within docs. 
    # ==================================================
    add_acronyms('ab', 'tit', 'pen', 'supp', 'bhd', 'indus', 'civ',); 

    # because acronyms are capital initialized words some may not work such as seq. 
    s/(seq\.)/$1,/g;
    s/(cert\.)/$1,/g;
    s/(disc\.)/$1,/g;
    s/(etc\.) ([A-Z])/$1. $2/g;	# ended with etc. might have problem. 
    
    # special cases.
    s/"If/" If/g;
    s/Id\.//g;			# This term is redundant?

    # remove the numbering at the beginning of paragraph. 
    s/\. ([SL]_[0-9]+)/\, $1/g;	# e.g: xxx. S_10. => xxx, S_10.
    s/\" ([SL]_[0-9]+)/", $1/g;
    s/([SL]_[0-9]+)\. ?([^A-Z])/$1\, $2/g; # change . to ,

    # Adjust headnotes and footnotes. 
    s/(FN_[0-9]+) ([A-Z])/$1\. $2/g;
    s/\. ([HF]N_[0-9]+)/\, $1/g;   
    s/([SL]_[0-9]+[.,;!'"])/$1 /g; # e.g: S_10,Abc => S_10, Abc

    # adjust marks. like .... ... etc. 
    s/\.\.\.\.?( [^A-Z])/, $1/g; # e.g: .... by => , by
    s/\.\.\.\.?( [A-Z])/\. $1/g; # e.g: .... By => . By
    s/\.\.\.?\.?([^ ])/\.$1/g;	 # e.g: ..., by => ., by

    # adjust numbers. 
    s/^[0-9]+\. //g;		# e.g: 1. The extent of ....
    s/([\"\.]) ([0-9]+[^\.])/$1, $2/g;
    s/([\"\.]) ([\(])/$1, $2/g;		   # e.g: A. (2nd) => A., (2nd)
    s/([\",\.][0-9]+)\. ([^A-Z])/$1, $2/g; # e.g: "2. xxx. => "2, xxx.
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
    # &loadAbbrev(); 
    my $sentences = get_sentences($parstr); 
    foreach $sentence (@$sentences) {
	# post-process the sentence.
	$sentence =~ s/  +/ /g;	# remove redundant spaces. 
	$sentence =~ s/\.\./\./g; # remove additional period mark. 
	$sentence =~ s/ ,/,/g;	  # remove space before ,. 
	$sentence =~ s/^\s+//g;	  # remove leading space. 
	$sentence =~ s/^\"([^\"]+)/$1/g; # remove unbalanced quotation. 

	# If sentence doesn't contain ending mark, add one. 
	if (($sentence =~ m/(^.*)([^\.\"\?\:])$/) && 
	    ($sentence !~ m/para_[0-9]+/)) {
	    $sentence = $1 . $2 . "."; 
	}
	print "$cnt:  " . $sentence . "\n"; 
	$cnt++;
    }
    return; 
}

# ==================================================
# @brief load abbrevations from file. 
# @param none. 
# @return none. 
# ==================================================
sub loadAbbrev {
    my $cnt = 0; 
    open(ABBFILE, "abbrev.abb");

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

1; 
