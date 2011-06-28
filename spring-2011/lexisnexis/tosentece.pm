#!/usr/bin/perl -w
# @file tosentence.pl
# @brief This script is to split paragraphs into sentences.
# We used the Lingua::EN::Sentence package for this task. 
# The unique difficulty for this task is to identify as many 
# variations as possible for the input data. So, we use the
# random sampling method to make this task sound. 
# 
# @author Simon Guo<shumin.guo@lexisnexis.com>
# @revision 1.0 06/15/2011, by Simon
# - Initially created.
# @revision 1.1 06/19/2011, by Simon
# - Added more acronyms and sentence variations by random sampling. 
# @comments Please update the revision log when you update
# this file, thanks. 

use Lingua::EN::Sentence qw( get_sentences add_acronyms get_EOS );

# the end of sentence.
my $EOS = &get_EOS(); 		# end of sentence separator. 

MAINLOOP:while (<STDIN>) {
    print $_; 
    chomp;
    # ======================================================================
    # --Preprocessing of the document.-- 
    # This process will be used to remove all the errors that can cause the
    # failure of the sentence cutting program. 

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

    print $_ . "\n\n";

    next MAINLOOP; 
    # ==================================================
    # Add acronyms within docs. 
    # ==================================================
    # do pre-processing to the paragraph, erase the unregular patterns.
    # add additional acronyms. 
    # ajust abbreviations. 
    add_acronyms('sec', 'am', 'jur', 'app', 'no', 'etc', 'id'); 
    add_acronyms('orig', 'rev', 'civ', 'stat', 'ann', 'mag', 'op', 'com', 'bl'); # 06/19/2011.
    add_acronyms('ab', 'tit', 'pen', 'supp', 'bhd', 'indus'); # 06/20/2011.

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
    #s/ ([0-9]+)\. / $1, /g;
    s/ I\.//g;			# remove numbering. 
    s/ II\.//g;
    s/ III\.//g;
    s/ IV\.//g;
    s/([\"\.]) ([0-9]+[^\.])/$1, $2/g;
    s/([\"\.]) ([\(])/$1, $2/g;		   # e.g: A. (2nd) => A., (2nd)
    s/([\",\.][0-9]+)\. ([^A-Z])/$1, $2/g; # e.g: "2. xxx. => "2, xxx.

    my $sentence = get_sentences($_); 
    foreach $sentence (@$sentence) {
	# post-process the sentence.
	$sentence =~ s/  +/ /g;	# remove redundant spaces. 
	$sentence =~ s/\.\./\./g; # remove additional period mark. 
	$sentence =~ s/ ,/,/g;	  # remove space before ,. 

	# If sentence doesn't contain ending mark, add one. 
	if (($sentence =~ m/(^.*)([^\.\"\?\:])$/) && 
	    ($sentence !~ m/PARAGRAPH_[0-9]+/)) {
	    $sentence = $1 . $2 . "."; 
	}
	print "\n" . $sentence . "\n"; 
    }
}

# //////////////////////////////////////////////////

# ==================================================
# @brief This function is used to process errors and 
# abnormal sentence formats within the document. 
# @category paragraph pre-processing. 
# @param 
# @param 
# @return 
# ==================================================
sub correctError {

}


# ==================================================
# @brief This subroutine processes acronyms. 
# @category paragraph post-processing. 
# @param 
# @param 
# @return 
# ==================================================
sub processAcronyms {

}

# ==================================================
# @brief Process specific cases. 
# @category paragraph pre-processing. 
# @param
# @return 
# ==================================================
sub processSpecific {

}


# ==================================================
# @brief Adjustment of sentences.  
# @category sentence post-processing. 
# @return 
# ==================================================
sub processSentence {

}
