require 5.6.1;

use strict;
use warnings;

use ExtUtils::MakeMaker;

# $Id$

# Usage: perl Makefile.PL [options]
# Options:
#  -f file   Use file for default configuration values (default is config.log)
#  -h        Print help
#  -k        Keep same values as config.log (or file specified with -f)

## Be sure to edit the version number in lib/Text/Restructured.pm for
## every release.
my $Version;
open PM, "lib/Text/Restructured.pm";
while (<PM>) {
    if (/\$VERSION\s*=/) {
	my $VERSION;		# N.B.: Needed so $VERSION passes strict
	$Version = eval $_;
    }
}
close PM;

use vars qw($opt_f $opt_h $opt_k);
my $OUTPUT_CFG_FILE = $opt_f = 'config.log';

use Getopt::Std;
usage() if ! getopts("f:hk") || $opt_h;

##### Do all user prompts first
my %CFG_LIST =
    (taint => {default => 'No',
	       desc => 'Run perl tainted (not required for safe operation)',
	       checkfail => \&yesno,
	       ,},
     defaultcss => {
	 default => 'None',
	 desc => 'URL for default cascading style sheet (or "none")',
	 explain => <<'EOS',
Generated documents need a style sheet to look good.  It is recommended
to serve a local copy of lib/Text/Restructured/default.css as an http URL.
You can also specify 'None', in which case the default stylesheet will
be embedded within every document.
EOS
	 checkfail => \&isurl,
         ,},
     docurl => {
	 default => 'None',
	 desc => 'URL where documentation will be installed (or "none")',
	 checkfail => \&isurl,
         ,},
     );
my @CFG_LIST = qw(defaultcss taint docurl);
warn "\@CFG_LIST and \%CFG_LIST have different number of elements"
    if @CFG_LIST != keys %CFG_LIST;

my %CONFIG;			# Our final configuration
my %DEFAULTS;			# Default values

# First read the config file if it exists
if (-f $opt_f) {
    open CF, $opt_f or die "Cannot open $opt_f";
    my %cfg = eval(join('',<CF>));
    @DEFAULTS{keys %cfg} = values %cfg;
    close CF;
}
else {
    # Set the defaults from %CFG_LIST
    @DEFAULTS{keys %CFG_LIST} = map($_->{default}, values %CFG_LIST);
}

# Do the user prompts
while (! $opt_k) {
    foreach my $cfg_item (@CFG_LIST) {
	my $message = $CFG_LIST{$cfg_item}{desc};
	$message = "$CFG_LIST{$cfg_item}{explain}\n$message"
	    if defined $CFG_LIST{$cfg_item}{explain};
	while (1) {
	    my $val = prompt ($message, $DEFAULTS{$cfg_item});
	    $val =~ s/^\s*(.*?)\s*$/$1/;
	    $CONFIG{$cfg_item} = $val;
	    last unless 
		defined $CFG_LIST{$cfg_item}{checkfail} &&
		&{$CFG_LIST{$cfg_item}{checkfail}}($CONFIG{$cfg_item});
	    $message = $CFG_LIST{$cfg_item}{desc};
	}
	$DEFAULTS{$cfg_item} = $CONFIG{$cfg_item};
    }

    print "\n";
    printsummary();

    my $okay = prompt("Does this look right?");
    last if ($okay !~ m/^[n0]/i);
}
if ($opt_k) {
    @CONFIG{@CFG_LIST} = @DEFAULTS{@CFG_LIST};
    printsummary();
}

# Add the prest version to the config
$CONFIG{version} = $Version;
# Add the perl executable to the config
$CONFIG{perl} = $^X;

# Write the configuration file (after saving the old one)
rename "$OUTPUT_CFG_FILE", "$OUTPUT_CFG_FILE.bak";
open (CL, ">$OUTPUT_CFG_FILE") or die "Cannot write to $OUTPUT_CFG_FILE.";
print CL map(qq('$_'=>'$CONFIG{$_}',\n), sort keys %CONFIG);
close CL;

##### Figure out what version of make to use.  We *require* GNU make.
my @path = split /:/, $ENV{PATH};
my ($make) = grep -x $_, map("$_/gmake", @path), map("$_/make", @path);

#### Write out all the .t files
my $generic_t = << 'EOS';
#!/usr/local/bin/perl5.8.8
use Slay::Makefile::Gress qw(do_tests);
do_tests('../../Common.smak', @ARGV);
EOS

my @TESTS;
my @generic_t_dirs = grep -d $_, <t/*>;
# Create generic .t files
foreach my $dir (@generic_t_dirs) {
    opendir DIR, $dir;
    my @need_t = grep s/\.init$/.t/, readdir(DIR);
    closedir DIR;
    foreach my $t (@need_t) {
	open T, ">$dir/$t";
	print T $generic_t;
	close T;
	push @TESTS, "$dir/$t";
    }
}

# Make sure PrestConfig.pm gets rebuilt
unlink "lib/Text/Restructured/PrestConfig.pm";

#### Finally, create the Makefile
# Get list of perl modules in lib subdirectory
chomp (my @pm_files =
       grep ! /\.svn|\bCVS\b|~\Z|\.PL\Z/, `find lib -type f`);
my %pm_files;
@pm_files{@pm_files} = map "blib/$_", @pm_files;

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
my $pm_filter = "$^X -pe 'END { print qq{\\044VERSION = $Version\\012} }'";
WriteMakefile(
    NAME           => 'Text::Restructured',
    AUTHOR         => 'Mark Nodine <mnodine@alum.mit.edu>',
    ABSTRACT       => 'Perl implementation of reStructuredText parser',
    VERSION        => $Version,
    EXE_FILES      => [qw(prest)],
    MAN1PODS       => {},
    MAN3PODS       => {},
    PREREQ_PM      => { 'Text::ASCIIMathML'=>0.5,
			'Slay::Makefile::Gress'=>0,
			'Safe::World'=>0,
			'HTML::Entities'=>0,
			}, # e.g., Module::Name => 1.1
    PL_FILES       => {'lib/Text/Restructured/PrestConfig.pm.PL' =>
		       'lib/Text/Restructured/PrestConfig.pm'},
    PM             => {'lib/Text/Restructured/PrestConfig.pm' =>
		       'blib/lib/Text/Restructured/PrestConfig.pm',
		       %pm_files},
#    PM_FILTER      =>  qq($^X -pe "END{print qq{our \\044VERSION=$Version\\;\\n}}"),
#    FIXIN          => 'cp',
    test           => { TESTS => join ' ',@TESTS },
    clean          => { FILES =>(join(' ', (@TESTS, map(/(.*)\.t$/ && "$1.run",
							@TESTS))) .
				 q[ t/Common.mk lib/Text/Restructured/PrestConfig.pm]) },
    dist           => { COMPRESS => 'gzip', SUFFIX => '.gz' },
    realclean      => { FILES => q(config.log config.log.bak) },
);

sub MY::libscan {
    my ($self, $path) = @_;
    return $path !~ /\.svn/ && $path;
}

sub MY::postamble {
    return <<'MAKE_FRAG';
FIXIN = $(PERLRUNINST) insertperl.pl

$(INST_SCRIPT)/prest : lib/Text/Restructured/PrestConfig.pm

.PHONY: doc
doc ::
	cd doc/src; $(MAKE)
MAKE_FRAG
}

#### Random support subroutines

# Prints a summary of the configuration
sub printsummary {
    print "Here is the summary of the configuration:\n";
    foreach my $cfg_item (@CFG_LIST) {
	print "  $CFG_LIST{$cfg_item}{desc}: $CONFIG{$cfg_item}\n";
    }
}

# This subroutine extracts usage information
sub usage {
  my($what,$end) = @_;
  $what = "Usage" if ! $what;
  my $print;
  if (open(ME,$0) == 1) {
    while (<ME>) {
      $print = 1 if /^# $what/o;
      $print = 0 if ! /^#/o || ($end && /^# $end/o);
      print substr($_,2) if $print;
    }
    close(ME);
  }
  else {
    print STDERR "Usage not available.\n";
  }
  exit;
}

# Checks for valid URL or "none"
sub isurl {
    my $fail = $_[0] !~ /^\w+:|none/i;
    print STDERR "Must be either a URL reference or 'None'\n"
	if $fail;
    return $fail;
}

# Checks for yes or no answer
sub yesno {
    my $fail = !($_[0] =~ s/^y.*/Yes/i || $_[0] =~ s/^n.*/No/i);
    print "Must be 'yes' or 'no'\n" if $fail;
    return $fail;
}
