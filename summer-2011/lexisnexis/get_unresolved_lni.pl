#!/usr/bin/perl
# This script is to resolve the unresolved LNI string. 
# The idea to resolve an unresolved LNI string is to build a lookup 
# dictionary, and then use this dictionary to lookup LNI using the 
# citation name as key. 
# 
while (<STDIN>) {
    while (m/<rfc:anchorcite( lni="[0-9A-Z\-]+")? rfctokenref=\"([A-Z0-9]+)\"[^>]*>(.*?)<\/rfc:anchorcite>/g) {
# 	print $2 . $1 . "\n";
	my $lni = $1; 
	my $reftoken = $2; 
	my $citestr = $3; 
	my $caserefid;
	my $anchorid; 

	# get case ref id, this is also unique. 
	if ($citestr =~ m/<lnci:caseref ID=\"([0-9A-Z]+)\"/) {
	    $caserefid = $1; 
	}

	# anchor id. this is unique. 
	if ($citestr =~ m/<ref:anchor id=\"([0-9A-Z]+)\"[^>]*>/) {
	    $anchorid = $1; 
	}

	# spanid this is unique. 
	my $spanid; 
	while ($citestr =~ m/spanid=\"([A-Z0-9]+)\"/g) {
	    $spanid = $1; 
	    print $spanid . "\n";
	}

	# print $reftoken . $ lni . "\n";
	print $caserefid . $lni . "\n";
	# print $spanid . $lni . "\n";
	# print $anchorid . $lni . "\n";
    }
}
