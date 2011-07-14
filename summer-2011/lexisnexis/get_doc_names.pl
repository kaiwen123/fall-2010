#!/usr/bin/perl 
# This script is used to build a dictionary of mappings between 
# case name and related LNI string. 
# We use MD5 to encode the case name and generate a equal length 
# string value as key to reference the LNI string.
# 
use Digest::MD5  qw(md5_base64 md5_hex);

while (<STDIN>) {
    my $casename, $lnistr; 

    # get case name. 
    if (m/<lncr:title [^>]+>(.*?)<\/lncr:title>/g) {
	$casename = $1; 
    }

    # get lnistr, which is also the file name of the case doc. 
    if (m/\/lnc\/(([0-9A-Z]{4}\-){4}[0-9A-Z]{5}\-[0-9A-Z]{2}\.xml)/) {
	$lnistr = $1; 
    }

    $casename =~ s/ //g;
    my $namehash = md5_hex $casename; 
    # print $namehash . "\t" . $casename . "\t" . $lnistr . "\n";
    print $namehash . "\t" . $lnistr . "\n";
}
