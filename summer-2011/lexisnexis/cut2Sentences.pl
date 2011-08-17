#!/usr/bin/perl
#<cut2Sentences>
# ================================================================
# After CA's are reduced, now cut Text to sentences 
# Input : Text file *_TextA.dat
# Output : *_TextB.dat 
# NOTE : not cut now: "...; that ...; that .." in long Sentences
# ================================================================

# BEGIN {
#   $PMDir = "/nfs/citation/uzhanpy/work/projects/Buddy/networkData/bin" ;
#   push @INC, $PMDir ;
# }

use commonTools ;

# ( $Root = $ARGV[0] ) =~ s/[_\.].*// ;

# if ( ! $Root ) {
#   print "\nUsage : cut2Sentences  BaseName\n\n" ;
#   exit ;
# }

# &getFileNames ;

# &fileIs ( "FileText", "$DIR_TextA/$Root\_TextA.dat.trim" ) ;
# &fileIs ( "OutFileText", "$DIR_TextS/$Root\_TextS.dat" ) ;

# &switchFileNames ;
# &ask2GoOn ;

&defineCommonITerms ;
# open ( IN, "$ARGV[0]" ) || die "Cannot Open <$FileText>" ;
# open ( OUT, ">$ARGV[1]" ) || die "Cannot Open <$OutFileText>" ;

LOOP:while (<STDIN>) {
  if ( /^ *$|^\$120:/ ) { 
    next LOOP ; 
  }
  chomp ;
  if ( /^([A-Z\d]{16}):/ ) {
    $NextThumb = $1 ;
    if ( $Thumb ) { 
      print "\n$Thumb\n" ;
      for $s ( @Sentences ) {
        $SCt++ ;
        print "$SCt: $s\n" ;
      }
    }
    @Sentences = () ;
    $ParaCt = 0 ;
    $SCt = 0 ;
    $Thumb = $NextThumb ; 
    next LOOP ;
  }  
  &cut2Sentences ( \@Sentences ) ;
}

close IN ;
# close OUT ;

# ==========================================================
# Cut one paragraph to Sentences (Paragraph is in $_ ) 
# Output is put to @{$ref_Sentences}
# ==========================================================
sub cut2Sentences  {
my $ref_Sentences = shift ;
  @S = @{$ref_Sentences} ;
  &removeQuotes ;

  # == Replace (.... ed. 1999); (1999), etc.  with [ ... ] 
#  s/\((.{2,8} ed\. \d\d\d\d)\)/[$1]/g ;
  s/\(([^()]{2,8} ed\.[^()]{0,8})\)/[$1]/g ;
  s/\((\d\d\d\d)\)/[$1]/g ;

  # ========================================================
  # Replace SBM's with zZZZ
  # ========================================================
  # == <." ...>, <.' ...>, and 4 dots followed by uppercase:
  s/(\S(?:\.|\?|\s*\.\s*\.\s*\.\s*\.\s*)["']{1,2}) +(['"]*[A-Z])/$1zZZZ$2/sg;

  # == 3 dots before " or ' followed by uppercase 
  s/(\. \. \. *\["']{1,3}) +(['"]*[A-Z])/$1zZZZ$2/sg; 

  # == <). > is SBD, Tested; (need to be followed by [A-Z] ) 
  # == Not : <". . . .>, <'. . . .> or <" )>, <' )>
  # == But Okay : <"'. otherCh> 
#  s/(\S[\]\)"'][\.\?]) +([^\.\)]{1,3})/$1zZZZ$2/sg;
  s/(\S[\]\)"'][\.\?](?:\s+n\d+\b)*) +([^\.\)]{1,3})/$1zZZZ$2/sg;
  # note: will miss : "... part of defendant. ( C_010  .)' In C_011 ..."

  # == <. . . . n1> or <. . . . [A-Z]>
  s/([a-z0-9']{2,}\s*\.\s*\.\s*\.\s*\. *)([A-Z]|n\d+ )/$1zZZZ$2/sg;

  # == <..) [A-Z][A-Za-z]...>
# s/([^()]{3,}\)) +(["']*[A-Z][a-zA-Z])/$1zZZZ$2/sg;
  s/([^()]{3,}\)(?:\s+n\d+\b)*) +(["']*[A-Z][a-zA-Z])/$1zZZZ$2/sg;

  # == <[ para. ] ...> and < ...] [A-Z]..>
  s/([^\[\]]{3,}\]) +(["']*[A-Z][a-zA-Z])/$1zZZZ$2/sg;

  # == NOTE : 4-letter words are required to end S 
  #      if there is an uppercase-letter
####  s/(\b(?:[A-Za-z]{4,}|[a-z]{2,}|\s*\([a-z]|[^:;]\s*\d+[a-z]{0,2}|[CT]_\d\d\d +)(?:\.|\?|\s*\.\s*\.\s*\.\s*\.)(?:\s+n\d+\b)*)( +['"][A-Z]|\([^\)]{3}|\[ ?[A-Z][^_]| +[A-Z]| *$)/$1zZZZ$2/sg;

s/(\b(?:[A-Za-z]{4,}|[a-z]{2,}|\s*\([a-z]|[^:;]\s*\d+[a-z]{0,2}|[CT]_\d\d\d +)(?:\.|\?|\s*\.\s*\.\s*\.\s*\.)(?:\s+n\d+\b)*)( +['"]*[A-Z]|\([^\)]{3}|\[ ?[A-Z][^_]| *$)/$1zZZZ$2/sg;


  # == If next S, starts with common terms, break it.
  s/\b([A-Za-z]{3,}\.) +((?:$ITerms) )/$1zZZZ$2/sg;  
  s/(\") +((?:$ITerms) )/$1zZZZ$2/sg;

  # == <. ) [A-Z]..> and <) . [A-Z]..>
  s/(\) *\. +|\. *?\)(?:\s+n\d+\b)* +)(['"]*[A-Z][A-Za-z ])/$1zZZZ$2/sg ;

  # == <C_001. [A-Z]...> , <C_001) [A-Z]...>
  s/([CT]_\d\d\d *[\.)]) +([A-Z])/$1zZZZ$2/sg ;

  # == < ...  . n4 ...>
  s/(\. +n\d+ +)(\S)/$1zZZZ$2/sg ;
 
  # == NOTE : Some sequences need to be restored, i.e. SBM removed == 
  # == Restore : cf. C_12 ...
  if ( /zZZZ/ ) {
    s/((?:zZZZ| )[Cc]f\. *)zZZZ/$1/g ;
    # == Restore : Rec. vol. III, ...
    s/\b([Vv]ol\.*) *zZZZ *([IVX]+)\b/$1 $2/g ;
  }

  # == Remove SBM between pairs of quotation marks 
  if ( /\"/ && /zZZZ/ ) {
    s/((?: |^|zZZZ)\"[^"]*?)zZZZ([^"]*?)zZZZ([^"]*?)zZZZ([^"]*?)zZZZ([^"]*?\"(?: |zZZZ|$))/$1 $2 $3 $4 $5/g;
    s/((?: |^|zZZZ)\"[^"]*?)zZZZ([^"]*?)zZZZ([^"]*?)zZZZ([^"]*?\"(?: |zZZZ|$))/$1 $2 $3 $4/g;
    s/((?: |^|zZZZ)\"[^"]*?)zZZZ([^"]*?)zZZZ([^"]*?\"(?: |zZZZ|$))/$1 $2 $3/g;
    s/((?: |^|zZZZ)\"[^"]*?)zZZZ([^"]*?\"(?: |zZZZ|$))/$1 $2/g;
  }

  # == Remove SBM between pairs of parenthases 
  if ( /\(/ && /zZZZ/ ) {
    s/(\([^()]*?)zZZZ([^()]*?)zZZZ([^()]*?)zZZZ([^()]*?\))/$1 $2 $3 $4/g;
    s/(\([^()]*?)zZZZ([^()]*?)zZZZ([^()]*?\))/$1 $2 $3/g;
    s/(\([^()]*?)zZZZ([^()]*?\))/$1 $2/g;
  }

  # ================================================================== 

  # == Now divide the text into Sentences 
  @SentenceArray = split ( / *zZZZ */, $_ ) ;
for $sentence (@SentenceArray) {
    print $sentence . "\n\n";
}
  $ParaCt++ ;
  # push ( @S, "PARAGRAPH$ParaCt" ) ;
  # push ( @S, @SentenceArray ) ;  
  # @{$ref_Sentences} = @S ;
}

# =======================================================================
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

# ================================================================
# Define terms that commonly occur at S. initial (stati.)
# ================================================================
sub defineCommonITerms {
  $ITerms  = "HEADNOTE_\d+_\d+|The|An|In|We|On|As|It|After|Because|He|But|Thus|She|However|While|This|Therefore|Thereupon|Thereafter|For|They|Although|Under|To|If|From|First|At|Accordingly|When|There|That|Rather|Not|None|Moreover|Indeed|His|Hence|Given|During|By|But|Both|According" ;
}
