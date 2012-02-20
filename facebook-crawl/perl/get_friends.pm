#!/usr/bin/perl 
# This file is used to extract friends from the friends lists. 

# This function is to extract friends from the friend list html file. 
sub get_friends {
    @friends = ();
    $_ = $_[0]; 
    chomp; 			# remove line breaking symbol.

    if (m/<div class="dualList fbProfileBrowserListContainer">(.*)<\/div>/g) {
	$_ = $1; 
    } else {
	# print "Error while extracting list, can't find pattern.";
	next LOOP; 
    }

    # now get friend information from list. 
    while (m/<a href="http:\/\/www.facebook.com\/([^"]+)"[^>]+eng_tid&quot;:([^,]+)[^>]+>(.*?)<\/a>/g) {
	my $f_nname = $1; 	# nick name. 
	my $f_id = $2; 		# fb unique id.
	my $f_name = $3; 
	
	# nick name. 
	if ($f_nname =~ m/(.*)\?ref=pb/) {
	    push @friends, $1; 
	} else {
	    push @friends, $f_id; 
	}
    }
    return @friends;
}

1; 
