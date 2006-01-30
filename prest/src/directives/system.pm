# $Id: system.pm 768 2006-01-28 03:33:28Z marknodine $
# Copyright (C) 2002-2005 Freescale Semiconductor, Inc.
# Distributed under terms of the GNU General Public License (GPL).

# This package implements the system directive for the perl implementation
# of reStructuredText.

=pod
=begin reST
=begin Description
Executes a system (shell) command and interpolates the results.
It has the following options:

``:lenient:``
  Causes the exit code for the subprocess to be ignored.
``:literal:``
  Interpret the returned value as a literal block.

If this option is not present, the return value is interpreted
as reStructuredText and is parsed again.

Any error returned by the shell generates a level 3 error message.  To see
the output of a command that is expected to generate an error, do::

  .. system:: <your command> 2>&1 | cat

The following defines are processed by the system directive:

-D trusted      Must be specified for system directives to be 
                processed.  This requirement is to prevent a system
                directive in a file written elsewhere from doing
                destructive things on your computer.
=end Description
=end reST
=cut

package RST::Directive::system;

BEGIN {
    RST::Directive::handle_directive('system', \&RST::Directive::system::main);
}

# Plug-in handler for system directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub main {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = RST::Directive::parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($args, $options) = map($dhash->{$_}, qw(args options));

    return RST::Directive::system_message($name, 3, $source, $lineno,
					   "Argument(s) required.", $lit)
	if $args =~ /^$/;

    if (! $Perl::safe) {
	# Create a safe compartment for the Perl to run
	use Safe;
	$Perl::safe = new Safe "Perl::Safe";
	# Grant privileges to the safe if -D trusted specified
	$Perl::safe->mask(Safe::empty_opset()) if $main::opt_D{trusted};
	# Copy in VERSION
	$Perl::Safe::VERSION = $main::VERSION;
	# Share opt_ variables, %ENV, STDIN, STDOUT, STDERR
	my @opts = grep(/opt_/, keys %main::);
	foreach (@opts) {
	    local *opt = $main::{$_};
	    *{"Perl::Safe::$_"} = *opt;
	}
	# Share RST and DOM subroutines
	foreach (keys %RST::) {
	    local *opt = $RST::{$_};
	    no strict 'refs';
	    *{"Perl::Safe::RST::$_"} = \&{"RST::$_"} if defined &{"RST::$_"};
	}
	foreach (keys %DOM::) {
	    local *opt = $DOM::{$_};
	    no strict 'refs';
	    *{"Perl::Safe::DOM::$_"} = \&{"DOM::$_"} if defined &{"DOM::$_"};
	}
	*Perl::Safe::ENV = \%ENV;
	*Perl::Safe::STDIN = *STDIN;
	*Perl::Safe::STDOUT = *STDOUT;
	*Perl::Safe::STDERR = *STDERR;
    }
    $Perl::Safe::TOP_FILE = $main::TOP_FILE;

    $args =~ s/\n/ /g;
    my $code = << "EOS";
my \$text = `\Q$args\E 2>/tmp/$$`;
open ERR, "/tmp/$$";
my \$errmsg = <ERR>;
close ERR;
unlink "/tmp/$$";
(\$text, \$?, \$errmsg)
EOS
    my ($text, $syserr, $errmsg) = $Perl::safe->reval($code);
    my $err = $@ =~ /trapped by/ ? "$@Run with -D trusted if you believe the code is safe" : $@;
    return RST::system_message(4, $source, $lineno,
			       qq(Error executing "$name" directive: $err.),
			       $lit)
	if $@;

    return RST::Directive::system_message($name, 3, $source, $lineno,
					  "Non-zero exit code: " .
					  ($errmsg || $text || '') )
	if $syserr && ! defined $options->{lenient};
    my $newsource = qq($name directive at $source, line $lineno);
    $text .= $errmsg if defined $errmsg;
    if (defined $options->{literal} && $text ne '') {
	my $lb = new DOM('literal_block', %RST::XML_SPACE,
			 source=>$newsource);
	$lb->append(newPCDATA DOM($text));
	return $lb;
    }
    elsif ($parent->{tag} eq 'substitution_definition') {
	chomp $text;
	$parent->append(newPCDATA DOM($text));
    }
    else {
	&RST::Paragraphs($parent, $text, $newsource, 1) if defined $text;
    }

    return;
}

1;
