#!/usr/bin/perl 
# This script is to get citation names of a legal document. 

# Citation names are embedded within <ref:cite4thisresource> and 
# </ref:cite4thisresource>

while(<STDIN>) {
    # get citation tags. 
    while (m/<ref:cite4thisresource[^>]*>(.*?)<\/?ref:cite4thisresource>/g) {
	my $citestr = $1 ;
	# print $citestr; 
	if ($citestr =~ m/<lnci:span[^>]*>(.*)<\/lnci:span>/g) {
	    my $citename = $1; 
	    print $citename . "\n";
	}
    }
}
