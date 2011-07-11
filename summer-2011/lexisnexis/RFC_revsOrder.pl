#!/serve/bin/perl
#<RFC_revsOrder>

#============!/serve/bin/perl -w

BEGIN {
  $RFCDir = "/nfs/citation/uzhanpy/work/projects/rfc" ;
  $PMDir = "/nfs/citation/uzhanpy/work/projects/Buddy/networkData/bin" ;
  push @INC, $RFCDir ;
  push @INC, $PMDir ;
}

use R_rfcTools ;
use R_rfcTools2 ;
use commonTools ;

( $Root = $ARGV[0] ) =~ s/[_\.].*// ;

if ( ! $Root ) {
  print "\nUsage : RFC_revsOrder  BaseName\n\n" ;
  exit ;
}

&getFileNames ;

&fileIs ( "File", "$DIR_TextS/$Root\_TextS.dat" ) ;
&fileIs ( "OutFile", "$DIR_RFC/$Root\_RFC" ) ;

&switchFileNames ;
&ask2GoOn ;

$s = "S" ;
$e = "E" ;

open ( IN, $File ) || die "Cannot Open <$File>" ;
open ( OUT, ">$OutFile" ) || die "Cannot Open <$OutFile>" ; 

# =======================================================
# Get the first thumbprint only
# =======================================================
$DocCt = 0 ;
PreLOOP:while (<IN>) {
  next PreLOOP if ( /^ *$/ ) ;
  chomp ;
  if ( /^([A-Z\d]{16})\b/ ) {
    $DocCt++ ;
    $Thumb = $1 ;
    last PreLOOP ;
  }
  else {
    die "Data ERROR : No Thumbprint Found for First Case" ;
  }
}

LOOP:while (<IN>) {
  next LOOP if ( /^ *$/ ) ;
  chomp ;
  if ( /^([A-Z\d]{16})\b/ ) {
    $DocCt++ ;
    $CurThumb = $Thumb ;
    $Thumb = $1 ;
    undef ( %RFCBoundary ) ;

    print OUT "\n$CurThumb:\n" ;

    &getRFCs ( $CurThumb, $SCt ) ; # @Sentences is passed internally
    @Sentences = () ;
    $SCt = 0 ;
    next LOOP ;
  }
  s/^(\d+): *// ;
  $GotSCt = $1 ;
#  next LOOP if ( /^ *$/ ) ;
  $Sentences[++$SCt] = $_ ; 
  if ( $GotSCt != $SCt ) {
    print "\n<<<$_>>>\n" ;
    die "Sentence Count Error : $Thumb ( $GotSCt != $SCt )\n" ;
  }
}

# ================================================
# Process the Last Document 
# ================================================
$CurThumb = $Thumb ;
$Thumb = $1 ;
undef ( %RFCBoundary ) ;
print OUT "\n$CurThumb:\n" ;
&getRFCs ( $CurThumb, $SCt ) ; 

print "Total $DocCt Cases Processed\n\n" ;
close IN ;
close OUT ;
# ================================================
# Main Process ends here 
# ================================================

sub getRFCs {
my ( $CurThumb, $CurrentSCt ) = @_ ;
### my $RFC_S ;
### my $RFC_E ;
 $Start ;
 $End ;
   $RFCCt = 0 ;
   undef ( %RFCBoundary ) ;

  while ( $S = $Sentences[$CurrentSCt] ) {
#print "\n= = = = = =\n  at S $CurrentSCt\n" ;
    if ( $S =~ /([CT]_\d\d\d)/ ) {
      $Start = $End = $CurrentSCt ;
      $Length = length ( $S ) ;
      if ( $Length < 1000 ) {
        &expandUp ;    # $Start passed internally
        &expandDown ;  # $End passed internally
      }
      &recordBoundaries ( $Start, $End ) ;
      $BoundaryChanged = "" ;
      # $MatchedST = "" ;
      if ( $Start < $CurrentSCt ) { 
        $CurrentSCt = $Start ; 
      }
    }
    $CurrentSCt-- ;
  }
#  &showBoundaries ;
#  &showBoundaries_T ;
  &reverseBoundariesArray ;
  &adjustBd ;
#  &showBoundaries ;
  &printRFCs ;
}

sub adjustBd {
my $i ;
  $CS = 1 ; $CE = 1 ;
  aLOOP:for $i ( 1 .. $RFCCt ) {
    $LS = $CS ; $LE = $CE ;
    $CS = $RFCBoundary{$s,$i} ; $CE = $RFCBoundary{$e,$i} ;
    next aLOOP if ( $CS > $LE ) ;
    $RFCBoundary{$s,$i} = ( $LS < $CS ) ? $LS : $CS ;
    $RFCBoundary{$e,$i} = ( $LE < $CE ) ? $CE : $LE ;
    $RFCBoundary{$s,($i-1)} = "" ;
    $RFCBoundary{$e,($i-1)} = "" ;
    $CS = $RFCBoundary{$s,$i} ;
    $CE = $RFCBoundary{$e,$i} ;
  }
}

sub recordBoundaries {
my ( $start, $end ) = @_ ;
  $RFCCt++ ;
  $RFCBoundary { $s, $RFCCt } = $start ;
  $RFCBoundary { $e, $RFCCt } = $end ;
}

sub reverseBoundariesArray {
my %RFCBoundary2 = () ;
my $r = 0 ;
  for ( $i=$RFCCt; $i>0; $i-- ) {
    $r++ ;
    $RFCBoundary2{$s,$r} = $RFCBoundary { $s, $i } ;
    $RFCBoundary2{$e,$r} = $RFCBoundary { $e, $i } ; 
  }
  %RFCBoundary = %RFCBoundary2 ; 
}

sub showBoundaries { 
  print "\n = = = RFC Boundaries = = =\n" ;
  for $i ( 1 .. $RFCCt ) {
    print "$RFCBoundary{$s,$i} , $RFCBoundary{$e,$i}\n" ;
  }
}

sub showBoundaries_T {
  print "\n = = = RFC Boundaries = = =\n" ;
  TLOOP:for $i ( 1 .. 10000 ) {
    if ( $RFCBoundary{$s,$i} || $RFCBoundary{$e,$i} ) { 
      print "$i: $RFCBoundary{$s,$i} , $RFCBoundary{$e,$i}\n" ;
    }
    else {
      last TLOOP ;
    }
  }
}

sub printRFCs {
my $n ;
  for $n ( 1 .. $RFCCt ) {
    if ( $RFCBoundary{$s,$n} && $RFCBoundary{$e,$n} ) {
      &printOneRFC ( $RFCBoundary{$s,$n}, $RFCBoundary{$e,$n} ) ; 
    }
  }
}

sub printOneRFC {
my ( $Start, $End, $PMessage ) = @_ ;
#  print OUT " <$PMessage> $CurrentSCt: " ;
  for $i ( $Start .. $End ) {
    if ( $Sentences[$i] =~ /^PARAGRAPH\d/ ) {
      print OUT " _P " ;
    }
    else {
      print OUT "$Sentences[$i] " ;
    }
  }
  print OUT "\n" ;
}

sub printOneRFC_forEval {
my ( $Start, $End, $PMessage ) = @_ ;
#  print OUT " <$PMessage> $CurrentSCt: " ;
  # ===============================
  #  For printing EVal Files only
  $st = $Start - 2 ;
  $ed = $End + 2 ;
  # =============================== 
  print OUT "($Start, $End) $CurThumb || " ;
  # for $i ( $Start .. $End ) {
  for $i ( $st .. $ed ) {
    if ( $i == $Start ) { print OUT "<<<| " ; }
    if ( $Sentences[$i] =~ /PARAGRAPH\d/ ) {
      print OUT " _P " ;
    }
    else {
      print OUT "$Sentences[$i] " ;
    }
    if ( $i == $End ) { print OUT "|>>> " ; }
  }
  print OUT "\n = = =\n\n" ;
}

# sub readTokFile {
#   while (<TOK>) {
#     chomp ;
#     if ( /^([A-Z\d]{16}):/ ) {
#       $TokThumb = $NextTokThumb ;
#       $NextTokThumb = $1 ;
#       @Tokens = () ;
#       return ;
#     }
#     else {
#       s/^([TC]_\d\d\d): *// ;
#       $Tokens { $1 } = $_ ;
#     }
#   }
# }

