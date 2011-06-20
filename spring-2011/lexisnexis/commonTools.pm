
# =================================================================
# Assign files name to Variables, also remember the Var. name for
#   Switching between local and systems files.
# =================================================================
sub fileIs {
my ( $Var, $File ) = @_ ;
  ${$Var} = $File ;  
  push @FileVar, $Var ;
}

# =================================================================
# If Env. Var. $Switch is NOT Set, switch all files to local names
# =================================================================
sub switchFileNames {
  for $V ( @FileVar ) { 
    if ( ! $Switch ) {
      ${$V} =~ s/^.*\/// ;
    }
    print "  $V = ${$V}\n" ;
  }
}

# =================================================================
# To Ask if Program should go on (with given setting, files, etc.)
# =================================================================
sub ask2GoOn {
  return if $NonInterActive ; 
  print "\n  OKay (Press Y to Go on) ? \n" ;
  $Ans = <STDIN> ;
  if ( $Ans =~ /^[Yy]/ ) {
    print STDERR "Go On, ...\n" ;
  } 
  else {
    print STDERR "\n   Exit Program !!\n\n" ;
    exit ;
  }
}

# =================================================================
# Functions to read Names of Dir. and Files from dataFile.dat
# dataFile.dat : has Files for Links, Text data, and Output
# Also read in env. var.
# =================================================================
sub getFileNames {
my $FileName = "/nfs/citation/uzhanpy/work/projects/Buddy/networkData/data_2/dataFile.dat" ;

  $Switch = $ENV{FLAG} ;
  $NonInterActive = $ENV{GO} ;

  print STDERR "File Switch = <$Switch> ;    To Set : setenv FLAG 1\n" ;
  print STDERR "NonInterActive = <$NonInterActive> ; To Set : setenv GO 1\n\n" ;

  open ( FNAME, $FileName ) || die "Cannot Open <$FileName>" ;
  RLOOP:while (<FNAME>) {
    chomp ;
    s/ *\#.*// ;
    s/ *$// ;
    next RLOOP if ( /^ *$/ ) ;
    s/\"//g ;
    ( $ID, $FileName ) = split ( / *= */, $_ ) ;
    if ( $ID eq "RootDIR" ) {
      ${$ID} = $FileName ;
      next RLOOP ;
    }
    ${$ID} = "$RootDIR/$FileName" ;
  }
  close FNAME ;
}

1 ;

