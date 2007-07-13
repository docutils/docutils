
use strict;
use warnings;

use Test::More;

use FindBin;

use lib "$FindBin::RealBin/../../tbin";
use Slay::Makefile 0.02;

# $Id$

=head1 NAME

gress

=head1 DESCRIPTION

This module provides support for running a set of regression tests
with the aid of a C<Slay::Makefile> file from a .t file.

=head1 USAGE

To use this module, the .t file calling it should contain something like:

  use FindBin;
  use lib "$FindBin::RealBin/.."; # So we can find gress.pm
  use gress;
  do_tests("../../SlayMakefile");

=head1 ROUTINES

=over

=item C<do_tests($makefile)>

Runs a series of tests using C<$makefile> as the C<Slay::Makefile> input.

=back

=head1 PROCESSING

Processing proceeds by the following steps:

=over

=item 1.

Search for a .init directory with the same base name as the .t file
invoking C<do_tests>.  Dies if there is no such directory.  For example,
if the file invoking c<do_tests> is C<cmdline.t>, it looks for
directory C<cmdline.init>.

=item 2.

Copy the .init directory to a .dir directory and C<cd> into that
directory.

=item 3.

Check for a file "skip.pl".  If it exists, execute it.  If it returns
a non-zero exit code, skip all the tests.  The text printed becomes
the reason for skipping the tests.

=item 4.

Use C<Slay::Makefile> to parse the C<$makefile> file.

=item 5.

Do a C<Slay::Makefile::make> of the C<pretest> target, if it exists.

=item 6.

Get a list of the dependencies of the C<test> target, which is a list
of files to create for the individual tests.

=item 7.

For each test C<t>,

=over

=item a.

Check for a file with the same base name as C<t> and the extension
C<.skip.pl>.  If it exists, execute it and skip the test if it returns
a non-zero exit code.  The text printed becomes the reason for
skipping the test.

=item b.

Run C<Slay::Makefile::make> for target C<t>.

=item c.

If no file C<t> was generated, report a failed test as failing to
build the file.  If C<t> was generated, then it should be empty for a
passing test.  Any text will be returned as the reason of failure for
the test.

=back

=back

=head1 ENVIRONMENT VARIABLES

=over

=item C<MKDBG>

If set, runs Slay::Makefile in debug mode.

=back

=cut

sub do_tests {
    my ($makefile) = @_;
    my $base = $FindBin::RealBin;
    my $lib = "$base/../../blib/lib";
    my ($myname) =  $FindBin::RealScript =~ /(.*)\.tt?$/;
    chdir $base;
    die "Error: No init directory for this test\n" unless -d "$myname.init";

    # First create the subdirectory for doing testing
    system "rm -rf $myname.dir" if -d "$myname.dir";
    system "cp -r $myname.init $myname.dir";

    chdir "$myname.dir";

    # Check to see if we need to skip all tests
    if (-f "skip.pl") {
	chomp (my $error = `$^X -I $lib skip.pl 2>&1`);
	plan(skip_all => "$error") if $?;
    }

    my %opts = (strict => 1) ;
    $opts{debug} = 1 if $ENV{MKDBG};
    my $sm = Slay::Makefile->new(\%opts);
    my $errs = $sm->parse("../../Common.smak");
    die join "\n", @$errs if @$errs;
    # Run the pretest target, if any
    eval { $sm->make('pretest') };
    $sm->maker->check_targets('test') unless @ARGV;

    # Get list of targets
    my ($rule, $deps, $matches) = $sm->maker->get_rule_info('test');
    my @tests = @ARGV ? @ARGV : defined $rule && $deps ? @$deps : () ;
    plan tests => 0+@tests;
  TEST:
    foreach my $test (@tests) {
	(my $base_test = $test) =~ s/\. .*? \z//x;
	if (-f "$base_test.skip.pl") {
	    # Check whether we need to skip this file
	    chomp (my $error = `$^X -I$lib $base_test.skip.pl 2>&1`);
	  SKIP:
	    {
		skip($error, 1) if $?;
	    }
	    next TEST if $?
	}
	$sm->make($test);
	my $ok = -r $test ? `cat $test` : "Failed to build $test";
	is ($ok, '', $test);
    }
}

1;
