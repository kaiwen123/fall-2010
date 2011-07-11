
# =======================================================================
# Tools for RFC 
# =======================================================================

sub expandUp {
my $ref_Start = shift ;
my $Start = ${$ref_Start} ;
my $n ;
  $SkipPARA = "" ;
  $ChangedUP = "" ;

  ExLOOP:for $n ( 1 .. 12 ) {
    last ExLOOP unless ( &uPOneLevel ( \$Start ) ) ;
    $BoundaryChanged = 1 ;
  }
  ${$ref_Start} = $Start ;
}

sub uPOneLevel {
my $ref_Start = shift ;
local $Start = ${$ref_Start} ;
local $ChangedUP = "" ;
my $pS = $Start - 1 ;
my $ppS = $Start - 2 ;
my $PreviousS = "" ;
my $PrePreviousS = "" ;

  return if ( $Start < 1 ) ;

  # NOTE : if not set, there will be warning by Perl
  if ( $pS > 0 ) {
    $PreviousS = $Sentences[$pS] ;
  }
  if ( $ppS > 0 ) {
    $PrePreviousS = $Sentences[$ppS] ;
  }

  $_ = $Sentences[$Start] ;

  # ==================================================
  # Current is PARAGRAPHnn and $SkipPARA is set
  #     Go up one line.
  # ==================================================
  if ( /\bPARAGRAPH\d\d\d/ ) {
    if ( $SkipPARA ) {
      &setSStartBack ;
    }
    $SkipPARA = "" ;
  }

  # ==================================================
  # When any BLOCKING term occurs, Stop
  # NOTE : " ... : HEADNOTE ... " is not blocked 
  #          may need to change back.
  # ==================================================
  elsif ( ( /^ *(We |This court)/ ) ||
       ( /(^|HEADNOTE) *((?:In |Under (the )*|The |Prusuant to ) *[CT]_\d\d\d)/ ) 
     )   {
    return 0 ;
  }

  # =====================================================
  # When condictions are met, (Skip PARA and) UP
  # =====================================================

  # ==========================================
  # S starts with C_nnn, or "(" 
  # NOTE : T_nnn has restrictions (see below) 
  # ==========================================
  elsif ( ( /^( *C_\d\d\d)/ ) ||
          ( /^( *['"]*\(.*)/ ) 
        )  {
    $SkipPARA = 1 ; # May go up more lines if ... see below
    &setSStartBack ; 
  }
  # ==========================================
  # Starts with T_nnn, but not " ...:", short, 
  #      of, if long, has "also C_nnn" in it.
  # ==========================================
  elsif ( ( /^( *T_\d\d\d)/ ) && 
          ( ! /: *$/ ) && 
          ( ( ! /( [A-z]+\s+[A-z]+\s+[A-z]+\s+[A-z]+)/ ) ||
            ( /(also .*[CT]_\d\d\d .*[A-z]+\s+[A-z]+\s+[A-z]+\s+[A-z]+)/ )
          )
        )  {
    $SkipPARA = 1 ; # May go up more lines if ... see below
    &setSStartBack ;
  }

  # = if C_nnn at beginning and previous L < 9 words and with
  #     "these" ....
  elsif ( ( $SkipPARA ) && 
          ( /\b(de novo|these|those|this|that|they|we|our|same)\b/i ) && 
          ( /^ *((?:\S+\s+){2,8}\S+\s*)$/ ) 
        )  {
    &setSStartBack ; 
    $SkipPARA = "" ;
  } 
  # = if C_nnn at beginning and previous L is short < 6 words 
  elsif ( ( $SkipPARA ) &&
          ( /^ *((?:\S+\s+){1,4}\S+\s*)$/ )
        )  {
    &setSStartBack ;
    $SkipPARA = "" ;
  }

  # =====================================================
  # When conditions are met, go UP
  # =====================================================
  # = 1 to 5 words before C_nnn and no "cf/compare"
  # NOTE : 5 words before T_nnn are NOT good 
  elsif ( ( /^ *((?:\S+\s+){0,4}\S+ *C_\d\d\d)/ ) &&
          ( ! /^(.{0,30}\b(cf\.|compare).{1,20}\()/i ) 
        )  {
    &setSStartBack ; 
  }
  elsif ( ( /^ *(See|Under th.{1,3} circumstances*)/ ) ||
          ( /^(.{0,30}\b(?:see |e.g. ).*[CT]_\d\d\d)/ ) ||
          ( /( *[Ii]n this (?:sense|respect))/ ) # only with C_nnn in line 
        ) {
    &setSStartBack ; 
  }
  elsif ( ( /^(.{0,30}\b(cf\.|compare).*[CT]_\d\d\d)/i ) &&
          ( ! /^(.{0,50}\()/ )  
        ) {
    &setSStartBack ;
  }

  # =====================================================
  # These check context in previous sentences.
  # =====================================================
  # === If last sentence or one before last ends with ":" 
  #       and is long (> 4 words)
  elsif ( ( $PreviousS =~ /(.+\s+\S+\s+\S+\s+\S+\s.*[:,] *)$/ ) ||
          ( $PrePreviousS =~ /(.+\s+\S+\s+\S+\s+\S+\s.*[:,] *)$/ )
        ) {
    &setSStartBack ; 
    $SkipPARA = 1 ;  # PARAGRAPHnn may be between the two lines.
  }

  # === ReSet the Starting Point ===
  ${$ref_Start} = $Start ;
  $ChangedUP ;
}

sub setSStartBack {
  return unless ( $Start > 1 ) ;
  $Start-- ;
###  $MatchedST .= " $1 | " ;
  $ChangedUP = 1 ;
}
 
1;

