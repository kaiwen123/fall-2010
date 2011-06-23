#!/usr/bin/perl
#
# @brief feature selection for classification.
# @param D input dataset.
# @param N Number of text corpus. 
# @param NoSample Number of samples.
# @param Threshold difference threshold.
# @return T Selection features ranked by weight.
# use strict; 			# enforce good programming rules. 
if ($#ARGV+1 != 3){
    print "Usage: \n";
    print "perl relief.pl D NoSample Threshold\n";
    exit;
}

# Variable initilization. 
# Pay attention to the initialization of hashes. 
%datasetfreq = ();	      # global feature/word frequency.
%dataset = ();		      # dataset token freq per data item/line.
%datalabel = ();	      # text class/label.
%W = ();		      # feature weight. 
@T = ();		      # selected features. 

# load in data from file. 
# This proces will build the global word frequency table. 
# The dataset table, which is actually one sample line, and the
# class/label table, which is extracted from the last  
# word each line. 
open(INPUT, "$ARGV[0]") || die("Error opening file $ARGV[0].");
$count = 0;
print %datasetfreq . "\n";
MAINLOOP:while(<INPUT>){
    chomp;
    @tokens = split(/ /, $_);
    $datalabel{$count} = pop(@tokens);
    
    # calc word frequencies.
    for(my $i = 0; $i < $#tokens; $i++) {
	# freq table for current line. 
	if(exists $dataset{$count}{$tokens[$i]}) {
	    $dataset{$count}{$tokens[$i]}++;
	} else {
	    $dataset{$count}{$tokens[$i]} = 1;
	}
	
	# a global word frequency table. 
	if (exists $datasetfreq{$tokens[$i]}) {
	    $datasetfreq{$tokens[$i]}++; 
	} else {
	    $datasetfreq{$tokens[$i]} = 1;
	}
    }
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
$NoSample = "$ARGV[1]";	# Nosample. 
for(my $i = 0; $i < $NoSample-1; $i++) {
    my $sel = int(rand($#dataset));
    push(@randsel, $sel);
}

# Second, update the value of feature weight according to: 
# Wj = Wj - diff(xj,nearHit)^2 + diff(xj,nearMissj)^2. 
for $randsel (@randsel) {
    $nearHitkey = &nearHit(\%dataset, \%datalabel, $randsel);
    $nearMisskey = &nearMiss(\%dataset, \%datalabel, $randsel);
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
	my $diff1 = &diff(\%dataset, $feature, $dkey, $nearMisskey); 
	my $diff2 = &diff(\%dataset, $feature, $dkey, $nearHitkey);
	my $diff = $diff1 * $diff1 - $diff2 * $diff2; 
	$W{$feature} += $diff;
	$dkey++;
    }
}

# Select features according to the input threshold. 
# And return.
$threshold = $ARGV[2];
for $feature (keys %W) {
    if($W{$feature} >= $threshold) {
	push(@T, $feature); 
	print "$feature is selected with weight $W{$feature}.\n";
    }
}

# ============================================================
# Subroutines and functions. 
# In order to Achieve efficiency, we pass parameters by reference
# for large objects such as %dataset and %datalabel.
# ============================================================
# 
# @brief calculation of nearHit, nearHit is the instance having
# minimum Euclidean distance among all the instances of the same
# class. 
# @param dataset, datalabel references and randomly selected key. 
# @return The selected data key meeting the nearHit rule. 
sub nearHit {
    print "near hit. \n";
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
	    my $distance = &distance($dsref, $dkey, $key);
	    if ($mindistance > $distance) {
		$mindistance = $distance; 
		$retkey = $key; 
	    }
	}
    }
    return $retkey;
}

# ============================================================
# @brief calculation of nearMiss, nearMiss is the instance having
# minimum Euclidean distance among all the instances of the different
# classes.
# @param dataset, datalabel references and randomly selected key. 
# @return The selected data key meeting the nearMiss rule. 
# ============================================================
sub nearMiss {
    print "near miss. \n";
    # $dsref - reference to the dataset reference. 
    # $dlref - reference to the dataset label. 
    # $dkey - Key of the randomly selected data item. 
    my ($dsref, $dlref, $dkey) = $_[0], $_[1], $_[2];
    my $mindistance = 10000.0; 	# supposed to be very large initially.
    my $class = ${$dlref}{$dkey}; 
    $retkey = ''; 
    for my $key (keys %{$dsref}) {
	my $class1 = ${$dlref}{$key}; 
	if ($class != $class1) {
	    my $distance = &distance($dsref, $dkey, $key);
	    if ($mindistance > $distance) {
		$mindistance = $distance; 
		$retkey = $key; 
	    }
	}
    }
    return $retkey; 
}

# ============================================================
# @brief calculate the Euclidean distance of two data items. 
# @param $dsref reference to the data set. 
# @param $key1 key of data item 1; 
# @param $key2 key of data item 2;
# ============================================================
sub distance {
    print "calculating distance @_\n"; 
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
    return sqrt($distance);
}

# @brief Calculate the difference of two data items, it is calculated
# according to following rule. Relief-F.
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

    return ${$dsref}{$dkey}{$feature} - ${$dsref}{$nearkey}{$feature};
    # below is the diff algorithm for Relief-F.
    # if (${$dsref}{$dkey}{$feature} == ${$dsref}{$nearkey}{$feature}) {
    # 	return 1; 
    # } else {
    # 	return 0; 
    # }
}
