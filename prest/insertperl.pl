# $Id: insertperl.pl.root 449 2005-05-31 19:07:04Z nodine $

use strict;

use Carp;
use Config;
use File::Basename qw(basename dirname fileparse);
use File::Spec;
use DirHandle;

use Text::Restructured::PrestConfig;

use vars qw($VERSION @ISA
            $Is_Mac $Is_OS2 $Is_VMS $Is_Win32 $Is_Dos $Is_VOS
            $Verbose %pm %static $Xsubpp_Version
            %Config_Override
           );

my $taintflag = $Text::Restructured::PrestConfig::TAINT =~ /^y/i ? ' -T' : '';
my $perl_exec = "$^X$taintflag";

my($does_shbang) = $Config{'sharpbang'} =~ /^\s*\#\!/;
foreach my $file (@ARGV) {
    local (*FIXIN);
    local (*FIXOUT);
    open FIXIN, $file or croak "Can't read '$file': $1";
    local $/ = "\n";
    chomp (my $line = <FIXIN>);
    next unless $line =~ s/^\s*\#!\s*//;     # Not a shbang file.
    # Now figure out the interpreter name.
    my($cmd,$arg) = split ' ', $line, 2;
    $cmd =~ s!^.*/!!;
    my $interpreter;
    if ($cmd eq "perl") {
	$interpreter = $perl_exec;
    }
    else {
	my(@absdirs) = reverse grep {File::Spec->file_name_is_absolute} File::Spec->path;
	$interpreter = '';
	my($dir);
	foreach $dir (@absdirs) {
	    if (maybe_command($cmd)) {
		warn "Ignoring $interpreter in $file\n" if $Verbose && $interpreter;
		$interpreter = File::Spec->catfile($dir,$cmd);
	    }
	}
    }
    # Figure out how to invoke interpreter on this machine.

    my($shb) = "";
    if ($interpreter) {
	print STDOUT "Changing sharpbang in $file to $interpreter" if $Verbose;
	# this is probably value-free on DOSISH platforms
	if ($does_shbang) {
	    $shb .= "$Config{'sharpbang'}$interpreter";
	    $shb .= ' ' . $arg if defined $arg;
	    $shb .= "\n";
	}
	$shb .= qq{
	    eval 'exec $interpreter $arg -S \$0 \${1+"\$\@"}'
		if 0; # not running under some shell
	} unless $Is_Win32; # this won't work on win32, so don't
    } else {
	warn "Can't find $cmd in PATH, $file unchanged"
	    if $Verbose;
	next;
    }

    unless ( open(FIXOUT,">$file.new") ) {
	warn "Can't create new $file: $!\n";
	next;
    }
    my($dev,$ino,$mode) = stat FIXIN;
    
    # Print out the new #! line (or equivalent).
    local $\;
    undef $/;
    print FIXOUT $shb, <FIXIN>;
    close FIXIN;
    close FIXOUT;

    unless ( rename($file, "$file.bak") ) {	
	warn "Can't rename $file to $file.bak: $!";
	next;
    }
    unless ( rename("$file.new", $file) ) {	
	warn "Can't rename $file.new to $file: $!";
	unless ( rename("$file.bak", $file) ) {
	    warn "Can't rename $file.bak back to $file either: $!";
	    warn "Leaving $file renamed as $file.bak\n";
	}
	next;
    }
    unlink "$file.bak";
} continue {
    close(FIXIN) if fileno(FIXIN);
    system("$Config{'eunicefix'} $file") if $Config{'eunicefix'} ne ':';;
}

sub maybe_command {
    my($file) = @_;
    return $file if -x $file && ! -d $file;
    return;
}

