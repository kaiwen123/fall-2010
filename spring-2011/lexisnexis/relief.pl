# @brief feature selection for classification.
# @param D input dataset.
# @param N Number of features to select.
# @param NoSample Number of samples.
# @param Threshold difference threshold.
# @return T Selection features ranked by weight.
sub relief {
    print @_;
    if ($#_ != 4){
        print 'Usage: \n';
        print 'perl relief.pl D S NoSample Threshold';
        exit;
    }

    # initialize the weight to zero.
    # @W : feature weight vector.
    # @T : selected features for return, size determined
    #      by threshold in the function parameter. 
    # @features : Names(Id) of features. 
    @W = ();
    @T = ();
    @features = ();

    # Open dataset file and read in all the datasets and 
    # put the datasets into a hash table with line number
    # as the key and the content in this line as value. 
    # Also, we assume that feature names are provided as 
    # a seperate file. But possible it was provided along 
    # with the datasets, e.g. the first line. 
    %datasets = {};
    open(INPUT, "$ARGV[0]");
    $count = 0;
    while(<INPUT>){
	$datasets{$count} = $_;
	$count++;
    }

    # readin features from a file. 
    $ff = 0;
    if ($ff) {
	open(FEATURES, "features.txt");
	while(<FEATURES>) {
	    push(@features, $_);
	}
    }

    # weight of all features initialized to 0.
    for($i = 0; i <= $#features; $i++) {
	push(@W, 0); 
    }

    # features are stored in the first line of datasets. 
    $f = $datasets{'0'}; 
    @features = split(/ /, $f);

    # random selection of samples.
    # And feature selection process. 
    # nearHit is the instance having minimum Euclidean distance
    # among all the instances of the same class. 
    # 
    # nearMiss is the instance having minimum Euclidean distance
    # among all the instances of the difference classes. 
    # 
    # Then weight was updated for each feature according to: 
    # Wj = Wj - diff(xj,nearHit)^2 + diff(xj,nearMissj)^2. 
    $NoSample = $ARGV[2];	# Nosample. 
    for($i = 0; $i < $NoSample; $i++) {
	my $sel = int(rand($count));
	if (exists $randsel{$sel}) {
	    # find the nearMiss and nearHit of this sample. 
	    # TODO.
	}
    }

    # Selection features according to the input threshold. 
    # And return.
    $threshold = $ARGV[3];	# threshold. 
    for($j = 0; $j <= $#features; $j++) {
	if($W[$j] >= $threshold) {
	    push(@T, $features[$j]); 
	    print "$features[$j] is selected with weight $w[$j].\n"
	}
    }
    return @T;
}
