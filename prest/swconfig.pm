#### Here is the configuration information for the config.prl file.

# Copyright (C) 2002-2005 Freescale Semiconductor
# Distributed under terms of the GNU General Public License (GPL).

package swconfig;

# Describes the configuration variables that need to be queried.
@ENVS = ('installdir' =>
	{default=>'/usr/local/bin',
	 desc=>'Location to install prest',
	 exec=>0,
	 },
	 'defaultcss' => 
	 {default=>'http://docutils.sourceforge.net/tools/stylesheets/default.css',
	  desc=>'URL for default cascading style sheet (recommended to serve local copy)',
	  },
	 'taint' =>
	 {default=>'No',
	  desc=>'Run perl tainted (not required for safe operation)',
	  checkfail=>\&yesno,
	  },
	 'chmod' =>
	 {default=>'/bin',
	  desc=>'Location of safe chmod',
	  exec=>1,
	  ,},
	 'cpio' =>
	 {default=>'/bin',
	  desc=>'Location of safe cpio',
	  exec=>1,
	  ,},
	 'dirname' =>
	 {default=>'/bin',
	  desc=>'Location of safe dirname',
	  exec=>1,
	  ,},
	 'egrep' =>
	 {default=>'/bin',
	  desc=>'Location of safe egrep',
	  exec=>1,
	  ,},
	 'find' =>
	 {default=>'/bin',
	  desc=>'Location of safe find',
	  exec=>1,
	  ,},
	 'mkdir' =>
	 {default=>'/bin',
	  desc=>'Location of safe mkdir',
	  exec=>1,
	  ,},
	 'perl' =>
	 {default=>'/bin',
	  desc=>'Location of safe perl',
	  exec=>1,
	  ,},
	 'rm' =>
	 {default=>'/bin',
	  desc=>'Location of safe rm',
	  exec=>1,
	  ,},
	 'tee' =>
	 {default=>'/bin',
	  desc=>'Location of safe tee',
	  exec=>1,
	  ,},
	 'which' =>
	 {default=>'/bin',
	  desc=>'Location of safe which',
	  exec=>1,
	  ,},
	);

chomp ($MY_DIR = `pwd`);
$MY_GRESS = "$MY_DIR/helpers";
# Describes the files that need to be configured.
%FILE = ('Makefile'=>
	 { init=>sub {qq(INSTALL_DIR = $CFGS{installdir}\n)} },
	 'src/PrestConfig.pm'=>
	 { init=>sub { << "EOS" } },
package PrestConfig;
\$DEFAULTCSS = "$CFGS{defaultcss}";
\$SAFE_PERL = "$CFGS{perl}/perl";
EOS
	 'src/insertperl.pl'=>
	 { init=>sub { << "EOS"} },
\$SAFE_PERL = "$CFGS{perl}/perl";
\$PERL_FLAGS = "@{[$CFGS{taint} =~ /^y/i ? '-T' : '']}";
EOS
	 'helpers/src/gresslib/copyfiles/Makefile'=>
	 { init=>sub { "GRESS_DIR = $MY_GRESS\n" } },
	 'helpers/src/gresslib/copyfiles/Makefile.master'=>
	 { init=>sub { "GRESS_DIR = $MY_GRESS\n" } },
	 'helpers/src/gress/gen_gress.prl'=>
	 { init=>sub { << "EOS" } },
\#!$CFGS{perl}/perl
\$PATH_REQ = "${\compute_path()}";
EOS

	 'helpers/src/gress/diffre.prl'=>
	 { init=>sub {qq(\#!$CFGS{perl}/perl\n)} },

	 'helpers/src/gress/run_gress.prl'=>
	 { init=>sub { << "EOS" } },
\#!$CFGS{perl}/perl
\$TEE_PATH = "$CFGS{tee}";
EOS
	 'tests/prest/Makefile'=>
	 { init=>sub {qq(GRESS_DIR = $MY_GRESS\n)} },
	 'tests/Makefile'=>
	 { init=>sub {
	     join('', `cat helpers/src/gresslib/copyfiles/Makefile.master`)
	   } },
	 'doc/src/Makefile'=>
	 { init=>sub { << "EOS" } },
GRESS_DIR = $MY_GRESS
PERL = $CFGS{perl}/perl
EOS
	 );

# Describes the list of instructions.
@INSTRUCTIONS = ('make all',
		 'make testall',
		 'make doc',
		 'make install',
    );

sub yesno {
    return !($_[0] =~ s/^y.*/Yes/i || $_[0] =~ s/^n.*/No/i);
}

sub compute_path {
    my $env;
    my %path;
    my %envs = @ENVS;
    foreach $env (keys %envs) {
	next unless $envs{$env}{exec};
	$path{$CFGS{$env}} = 1;
    }
    return join(":",sort keys %path);
}

# If present, a WRAPUP subroutine gets called at the end.
#sub WRAPUP {
#    print "Configuring from $CFGS{gress}/lib/copyfiles/Makefile.master into tests/Makefile\n";
#    system ("cp -f $CFGS{gress}/lib/copyfiles/Makefile.master tests/Makefile");
#}

1;
