#!/usr/local/bin/perl -w
use strict;

my $file = shift;

# Read the config file (if it exists)
my $cf = 'config.log';
open CF, $cf or die "Cannot open $cf; rerun 'perl Makefile.PL'";
my %CONFIG = eval(join('',<CF>));
close CF;

open OUT, ">$file" or die "Cannot write to $file";
# My output
my $output = << 'EOS';
package Text::Restructured::PrestConfig;

$DEFAULTCSS = "${CONFIG{defaultcss}}";
$SAFE_PERL = "${^X}";
$TAINT = "${CONFIG{taint}}";
$VERSION = "${CONFIG{version}}";

1;
EOS
# Substitute in for ${perlvar} references
$output =~ s/(\$ \{ \S+ \})/eval "qq($1)"/gex;
print OUT $output;
close OUT;
