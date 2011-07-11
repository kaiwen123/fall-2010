
sub expandDown {
my $ref_End = shift ;
my $End = ${$ref_End} ;
my $n ;
  $SkipPARA = "" ;
  $ChangedDown = "" ;
  ExLOOP:for $n ( 1 .. 12 ) {
    last ExLOOP unless ( &downOneLevel ( \$End ) ) ;
    $BoundaryChanged = 1 ;
  }
  ${$ref_End} = $End ;
}

sub downOneLevel {
my $ref_End = shift ;
local $End = ${$ref_End} ;
local $ChangedDown = "" ;
my $nS = $End + 1 ;
my $nnS = $End + 2 ;
  $_ = $Sentences[$End] ;

  if ( /PARAGRAPH\d/ ) {
    return 0 unless ( $SkipPARA ) ;
    $SkipPARA = "" ;
    &setSEndForward ;
  }

  # ========================================================
  # If current line ends with ":" or ","
  # ========================================================
  elsif ( /[,:] *$/ ) {
    $SkipPARA = 1 ;
    &setSEndForward ; 
  }
  # =========================================================
  # These are NOT used now.  May use if lager text is needed
  # Also: if C_nnn is close to end (5 ch.), and next S has:
  #    "In this case ..."
  # =========================================================
  # elsif ( ( $Sentences[$nS] =~ /(This|Those|These are) / ) && 
  #         ( $Sentences[$nS] !~ /(This (?:court|case))/ ) &&
  #         ( $Sentences[$nS] =~ /.{60}/ )
  #       ) {
  #   &setSEndForward ; 
  # }

  ${$ref_End} = $End ;
  $ChangedDown ;
}

sub setSEndForward {
  $End++ ;
###  $MatchedST .= " $1 | " ;
  $ChangedDown = 1 ;
}

1;

