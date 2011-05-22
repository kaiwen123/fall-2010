#!/usr/bin/perl -w

open (FILE, "out1") || die " Cannot find file";
open (OUT, ">sentence") || die " Cannot find file";

while (<FILE>) {

if ($_ =~ /(.{20}\.)/g) {

print OUT $1."\n";

}


}
