# @brief feature selection for classification.
# @param D input dataset.
# @param N Number of text corpus. 
# @param NoSample Number of samples.
# @param Threshold difference threshold.
# @return T Selection features ranked by weight.
if ($#_ != 4){
    print 'Usage: \n';
    print 'perl relief.pl D N NoSample Threshold';
    exit;
}

# Variable initilization. 
%datasetfreq = {};		# global feature/word frequency.
%dataset = {};		# dataset token freq per data item/line.
%datalabel = {};		# text class/label.
%W = {};			# feature weight. 
@T = ();			# selected features. 

# load in data from file. 
# This proces will build the global word frequency table. 
# The dataset table, which is actually one sample line. 
# And the class/label table, which is extracted from the last 
# word each line. 
open(INPUT, "$ARGV[0]");
$count = 0;
while(<INPUT>){
    print $_;
    @tokens = split(/ /, $_);
    
    # calc word frequencies.
    %wordfreq = {};
    for(my $i = 0; $i < $#tokens - 1; $i++) {
	# freq table for current line. 
	if(exists $wordfreq{$tokens[$i]}) {
	    $wordfreq{$tokens[$i]}++;
	} else {
	    $wordfreq{$tokens[$i]} = 1;
	}

	# a global word frequency table. 
	if (exists $datasetfreq{$tokens[$i]}) {
	    $datasetfreq{$tokens[$i]}++; 
	} else {
	    $datasetfreq{$tokens[$i]} = 1;
	}
    }
    $dataset{$count} = %wordfreq; 
    $datalabel{$count} = $tokens[$#tokens];

    $count++;
}

# Initialize the weight vector into zeros. 
for my $key (keys %datasetfreq) {
    $W{$key} = 0.0;
}

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
# 
# First generate $NoSample number of random numbers. 
@randsel = ();
$NoSample = $ARGV[2];	# Nosample. 
for(my $i = 0; $i < $NoSample; $i++) {
    my $sel = int(rand($#dataset));
    push(@randsel, $sel);
}

# Second, update the value of feature weight according to the 
# relief rule. 
for $sel (@randsel) {
    %data = $dataset{$sel};	# randomly selected data. 
    %nearHit = nearHit();	# nearHit. 
    %nearMiss = nearMiss();	# nearMiss.
    # update the weight for each feature. 
    for(my $i = 0; $i < $#dataset; $i++) {
	my $dis1 = distance(%data, %nearMiss); 
	my $dis2 = distance(%data, %nearHit);
	$diff = $dis1 * $dis1 - $dis2 * $dis2; 
	$W{$i} += $diff; 
    }
}

# Select features according to the input threshold. 
# And return.
$threshold = $ARGV[3];
for $feature (keys %W) {
    if($W{$feature} >= $threshold) {
	push(@T, $feature); 
	print "$features is selected with weight $W{$feature}.\n";
    }
}

print "@T";

# nearhit. 
sub nearHit {

}

# nearmiss. 
sub nearMiss {
    
}

# diff function. 
# The Euclidean distance of the two vectors are calculated. 
sub distance {
    %data1, %data2 = $_[0], $_[1]; 
    $distance = 0.0; 
    for $key (keys %data1) {
	if (exists $data2{$key}) {
	    my $tmp = $data1{$key} - $data2{$key}; 
	    $distance += $tmp * $tmp;
	} else {
	    $distance += $data1{$key} * $data1{$key};
	}
    }
    for $key (keys %data2) {
	if (undef $data1{$key}) {
	    $distance += $data2{$key} * $data2{$key}; 
	}	
    }
    return $distance;
}
