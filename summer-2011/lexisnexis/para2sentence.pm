#!/usr/bin/perl
# @brief This script implements cutting of paragraphs 
# into sentences. This module file will be used by 
# the xml2text.pl file. 
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
    if ($parstr =~ m/^ *$/) { return; }
    chomp;

    # remove quotations in paragraph. 
    #&removeQuotes; 

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
    my $file = "abbrev.abb";
    # &loadAbbrev($file); 
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
sub removeQuotes {
my $L ;
my $QLength ;
my @QCts ;
  $QLength = "" ;
  @QCts = ( /(\")/g ) ;
  return unless ( $#QCts > -1 ) ;

  $PLength = length ( $_ ) ;

  # =======================================================
  # Remove " at Beginning if no other " found in string
  # =======================================================
  if ( ( $#QCts == 0 ) &&
       ( /^(?:HEADNOTE_\d+_\d+)* *\"/ )
     ) {
    s/\"// ;
  }

  # =======================================================
  # Remove " at both ends
  # =======================================================
  if ( ( /^(?:HEADNOTE_\d+_\d+)* *\"/ ) &&
       ( /\"[^A-Za-z]{0,5}$/ )
     ) {
    # =====================================================
    # Do nothing if first "..." not includes WHOLE String
    # =====================================================
    if ( $#QCts > 1 ) {
      return ;
    }
    else {
      # ===================================================
      # If conditions are met, remove " at both ends
      # ===================================================
      if ( ( $PLength > 1000 ) ||
           ( /(?:...(?:HEADNOTE_\d+_\d+|CANOTE)..+?){2}/ ) ||
           ( /(?:[A-Za-z]{3}[\.\?] .+?){5}/ ) ||
           ( /\b(?:I |Q\. )/ ) ||
           ( /\b(?:you|he) /i )
         ) {
        s/^(HEADNOTE_\d+_\d+ *)*\"/$1/ ;
        s/\"([^A-Za-z]{0,5})$/$1/ ;
      }
    }
  }

  return if ( $PLength < 1200 ) ;
  # =======================================================
  # Remove " " around long strings : 
  # I.e. String : longer than 2000; or
  #               longer than 1200 and NOT followed by "("
  # =======================================================
  if ( / \"([^"]{1200,})\"[ \.,\?]*(.{0,6})/ ) {
    $LongQuate = $1 ;
    $TrailingTxt = $2 ;
    $QLength = length ( $LongQuate ) ;
    if ( ( $QLength > 2000 ) ||
         ( $TrailingTxt !~ /\(/ )
       ) {
      s/\"\Q$LongQuate\E\"/$LongQuate/g ;
    }
  }
}

1; 
