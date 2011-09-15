#!/usr/bin/perl 
# This file is used to extract friends from the friends lists. 

# This function is to extract friends from the friend list html file. 
sub get_friends {
    $_ = $_[0]; 
    chomp; 			# remove line breaking symbol.
    my $fnd_lst = "";
    # print $_ . "\n";
    if (m/<div class="fbProfileBrowserList normalList">(.*)<\/div>/g) {
	$fnd_lst = $1; 
    } else {
	# print "Error while extracting list, can't find pattern.";
	next LOOP; 
    }

    # now get friend information from list. 
    while ($fnd_lst =~ m/<a href="http:\/\/www.facebook.com\/([^"]+)"[^>]+eng_tid&quot;:([^,]+)[^>]+>(.*?)<\/a>/g) {
	my $f_nname = $1; 	# nick name. 
	my $f_id = $2; 		# fb unique id.
	my $f_name = $3; 
	
	# nick name. 
	if ($f_nname =~ m/(.*)\?ref=pb/) {
	    $f_nname = $1; 
	} else {
	    $f_nname = "";
	}

	print $f_id . ":" . $f_nname . ":" . $f_name . "\n";
    }
}

1; 
