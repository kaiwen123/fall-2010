while (<STDIN>) {
    if (m/\/lnc\/(([0-9A-Z]{4}\-){4}[0-9A-Z]{5}\-[0-9A-Z]{2}\.xml)/) {
	print $1 . "\n";
    }
}
