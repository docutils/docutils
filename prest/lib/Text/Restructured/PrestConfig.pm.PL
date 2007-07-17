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
my $output = join '',<DATA>;
# Substitute in for ${perlvar} references
$output =~ s/(\$ \{ \S+ \})/my $v = eval "qq($1)"; $v || $1/gex;
print OUT $output;
close OUT;
__END__
package Text::Restructured::PrestConfig;

$DEFAULTCSS = q[${CONFIG{defaultcss}}];
$SAFE_PERL = q[${^X}];
$TAINT = q[${CONFIG{taint}}];
$VERSION = q[${CONFIG{version}}];
$DOCURL = q[${CONFIG{docurl}}];

1;
