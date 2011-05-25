#!/usr/bin/perl -w
# This script is to split paragraphs into sentences.
# We used the Lingua::EN::Sentence package for this task. 
# 
use Lingua::EN::Sentence qw( get_sentences add_acronyms );

open (INPUT, "$ARGV[0]") || die " Cannot find file";
# open (OUT, ">sentence") || die " Cannot find file";

while (<INPUT>) {
    my $sentence = get_sentences($_); 
    foreach $sentence (@$sentence) {
	print $sentence . "\n"; 
    }
}

close INPUT;
