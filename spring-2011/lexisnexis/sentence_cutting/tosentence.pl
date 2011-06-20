#!/usr/bin/perl -w
# @file tosentence.pl
# @brief This script is to split paragraphs into sentences.
# We used the Lingua::EN::Sentence package for this task. 
# @author Simon Guo<shumin.guo@lexisnexis.com>
# @revision 1.1 
# @comments Please update the revision log when you update
# this file, thanks. 

use Lingua::EN::Sentence qw( get_sentences add_acronyms get_EOS );

# open (INPUT, "$ARGV[0]") || die " Cannot find file";
# open (OUT, ">sentence") || die " Cannot find file";
my $EOS = &get_EOS(); 		# end of sentence separator. 

while (<STDIN>) {
    # do pre-processing to the paragraph, erase the unregular patterns.
    # add additional acronyms. 
    # ajust abbreviations. 
    add_acronyms(q/[0-9]+/, 'Sec', 'Am', 'Jur', 'App', 'No', 'Etc', 'Id'); 
    add_acronyms('orig', 'Rev', 'Civ', 'Stat', 'Ann', 'Mag', 'Op', 'Com', 'Bl'); # 06/19/2011.
    add_acronyms('Ab', 'tit');

    # because acronyms are capital initialized words some may not work such as seq. 
    s/(seq\.)/$1,/g;
    s/(cert\.)/$1,/g;
    s/(disc\.)/$1,/g;
    s/(etc\.) ([A-Z])/$1. $2/g;	# ended with etc. might have problem. 
    
    # special cases.
    s/"If/" If/g;
    s/Id\.//g;			# This term is redundant?
    s/([0-9a-z]+\."?)([A-Z])/$1 $2/g; # Add space between sentences. 

    # remove the numbering at the beginning of paragraph. 
    s/\. ([SL]_[0-9]+)/\, $1/g;	# e.g: xxx. S_10. => xxx, S_10.
    s/\" ([SL]_[0-9]+)/", $1/g;
    s/([SL]_[0-9]+)\. ?([^A-Z])/$1\, $2/g; # change . to ,

    # Adjust headnotes and footnotes. 
    s/\. ([HF]N_[0-9]+)/\, $1/g;   
    s/([SL]_[0-9]+[.,;!'"])/$1 /g; # e.g: S_10,Abc => S_10, Abc

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
	# process the sentence before print out.
	$sentence =~ s/  +/ /g;	# remove redundant spaces. 
	print "\n" . $sentence . "\n"; 
    }
}

# close INPUT;
