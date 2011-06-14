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
    # 
    # remove pattern like: "Commssioner.While", no space between two 
    # sentences. 
    s/([A-Z]?[a-z]+)\.([A-Z][a-z]+)/$1\. $2/g;

    # add additional acronyms. 
    add_acronyms(q/[0-9]+/, 'Sec', 'Am', 'Jur', 'App', 'No', 'Etc', 'Id'); 
    # because acronyms are capital initialized words some may not work such as seq. 
    s/(seq\.)/$1,/g;
    s/(cert\.)/$1,/g;
    s/(disc\.)/$1,/g;

    # remove the numbering at the beginning of paragraph. 
    s/^[0-9]+\.//g;		# e.g: 1. The extent of ....
    s/\. ([SL]_[0-9]+)/\, $1/g;	# e.g: xxx. S_10. => xxx, S_10.
    s/\" ([SL]_[0-9]+)/", $1/g;
    s/\. (FN_[0-9]+)/\, $1/g;
    s/([SL]_[0-9]+[.,;!'"])/$1 /g; # e.g: S_10,Abc => S_10, Abc
    # s/([\.\"]) ([0-9]+ Am\.)/$1, $2/g; # e.g: xxx." 25 Am. Jur.
    s/([\"\.]) ([0-9]+)/$1, $2/g;
    s/([\"\.]) ([\(])/$1, $2/g;		   # e.g: A. (2nd) => A., (2nd)
    s/([\",\.][0-9]+)\./$1,/g;		   # e.g: "2. xxx. => "2, xxx.
    s/([SL]_[0-9]+)\. ?([^A-Z])/$1\, $2/g; # change . to ,

    my $sentence = get_sentences($_); 
    foreach $sentence (@$sentence) {
	# remove lines that only contains numbering. 
	$sentence =~ s/  +/ /g; # multiple spaces to one space. 
	print STDOUT "\n" . $sentence . "\n"; 
    }
}

# close INPUT;
