#!/usr/bin/perl
#
# @brief feature selection for classification.
# @param D input dataset.
# @param N Number of text corpus. 
# @param NoSample Number of samples.
# @param Threshold difference threshold.
# @return T Selection features ranked by weight.
use strict; 			# enforce good programming rules. 
if ($#ARGV != 4){
    print 'Usage: \n';
    print 'perl relief.pl D N NoSample Threshold';
    exit;
} else {
    print "@ARGV"
}

# Variable initilization. 
%datasetfreq = {};	      # global feature/word frequency.
%dataset = {};		      # dataset token freq per data item/line.
%datalabel = {};	      # text class/label.
%W = {};		      # feature weight. 
@T = ();		      # selected features. 

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
    $datalabel{$count} = pop(@tokens);
    
    # calc word frequencies.
    %wordfreq = {};
    for(my $i = 0; $i < $#tokens; $i++) {
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

    $count++;
}

# Initialize the weight vector into zeros. 
for my $key (keys %datasetfreq) {
    $W{$key} = 0.0;
}

# random selection of samples.
# And feature selection process. 
#
# First generate $NoSample number of random numbers. 
@randsel = ();
$NoSample = $ARGV[2];	# Nosample. 
for(my $i = 0; $i < $NoSample; $i++) {
    my $sel = int(rand($#dataset));
    push(@randsel, $sel);
}

# Second, update the value of feature weight according to: 
# Wj = Wj - diff(xj,nearHit)^2 + diff(xj,nearMissj)^2. 
for $sel (@randsel) {
    $nearHitkey = &nearHit(\%dataset, \%datalabel, $sel);
    $nearMisskey = &nearMiss(\%dataset, \%datalabel, $sel);
    # 
    # update the weight for each feature.
    # It updates the weights of the features that are initialized to
    # zero in the beginning based on an intuitive idea that a feature
    # is more relevant if it distinguishes between an instance and its
    # near Miss, and less relevant if it distinguishes between an
    # instance and its near Hit 
    # 
    my $dkey = 0; 
    for my $feature (keys %W) {
	my $diff1 = diff(\%dataset, $feature, $dkey, $nearMisskey); 
	my $diff2 = diff(\%dataset, $feature, $dkey, $nearHitkey);
	my $diff = $diff1 * $diff1 - $diff2 * $diff2; 
	$W{$feature} += $diff; 	
	$dkey++;
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


# ============================================================
# 
# Subroutines and functions. 
# In order to Achieve efficiency, we pass parameters by reference
# for large objects such as %dataset and %datalabel.
# 
# ============================================================
# 
# @brief calculation of nearHit, nearHit is the instance having
# minimum Euclidean distance among all the instances of the same
# class. 
# @param dataset, datalabel references and randomly selected key. 
# @return The selected data key meeting the nearHit rule. 
sub nearHit {
    # $dsref - reference to the dataset reference. 
    # $dlref - reference to the dataset label. 
    # $dkey - Key of the randomly selected data item. 
    my ($dsref, $dlref, $dkey) = $_[0], $_[1], $_[2];
    my $mindistance = 10000.0; 	# supposed to be very large.
    my $class = ${$dlref}{$dkey}; 
    $retkey = ''; 
    for my $key (keys %{$dsref}) {
	my $class1 = ${$dlref}{$key}; 
	if ($class == $class1) {
	    $distance = &distance($dsref, $dkey, $key);
	    if ($mindistance > $distance) {
		$mindistance = $distance; 
		$retkey = $key; 
	    }
	}
    }
    return $retkey;
}

# @brief calculation of nearMiss, nearMiss is the instance having
# minimum Euclidean distance among all the instances of the different
# classes.
# @param dataset, datalabel references and randomly selected key. 
# @return The selected data key meeting the nearMiss rule. 
sub nearMiss {
    # $dsref - reference to the dataset reference. 
    # $dlref - reference to the dataset label. 
    # $dkey - Key of the randomly selected data item. 
    my ($dsref, $dlref, $dkey) = $_[0], $_[1], $_[2];
    my $mindistance = 10000.0; 	# supposed to be very large.
    my $class = ${$dlref}{$dkey}; 
    $retkey = ''; 
    for my $key (keys %{$dsref}) {
	my $class1 = ${$dlref}{$key}; 
	if ($class != $class1) {
	    $distance = &distance($dsref, $dkey, $key);
	    if ($mindistance > $distance) {
		$mindistance = $distance; 
		$retkey = $key; 
	    }
	}
    }
    return $retkey; 
}


# @brief calculate the Euclidean distance of two data items. 
# @param $dsref reference to the data set. 
# @param $key1 key of data item 1; 
# @param $key2 key of data item 2;
sub distance {
    print "calculating distance @_"; 
    my ($dsref, $key1, $key2) = $_[0], $_[1], $_[2]; 
    my (%data1, %data2) = ${$dsref}{$key1}, ${$dsref}{$key2};
    $distance = 0.0; 
    for my $key (keys %data1) {
	if (exists $data2{$key}) {
	    my $tmp = $data1{$key} - $data2{$key}; 
	    $distance += $tmp * $tmp;
	} else {
	    $distance += $data1{$key} * $data1{$key};
	}
    }
    for my $key (keys %data2) {
	if (undef $data1{$key}) {
	    $distance += $data2{$key} * $data2{$key}; 
	}	
    }
    return $distance;
}


# @brief Calculate the difference of two data items, it is calculated
# according to following rule. 
# diff(f, E1, E2) = 0 if value(f, E1) == value(f, E2); 
# diff(f, E1, E2) = 1 if value(f, E1) != value(f, E2);
# 
# For more details, please refer to (page 106): 
# A feature set measure based on Relief (Antonio Arauzo-Azofra)
# 
# @param $dsref reference to the data set. 
# @param $feature the feature to compare. 
# @param $dkey key for data to compare with nearxxx.
# @param $nearkey key for nearMiss or nearHit. 
# @return difference of two data item on $feature. 
sub diff {
    my ($dsref, $feature, $dkey, $nearkey) = @_;
    if (${$dsref}{$dkey}{$feature} == ${$dsref}{$nearkey}{$feature}) {
	return 1; 
    } else {
	return 0; 
    }
}
