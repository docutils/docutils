# $Id: system.pm 768 2006-01-28 03:33:28Z marknodine $
# Copyright (C) 2002-2005 Freescale Semiconductor, Inc.
# Distributed under terms of the Perl license, which is the disjunction of
# the GNU General Public License (GPL) and the Artistic License.

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

package Text::Restructured::Directive::system;

BEGIN {
    Text::Restructured::Directive::handle_directive
	('system', \&Text::Restructured::Directive::system::main);
}

use vars qw($DOM);
BEGIN {
    *DOM = "Text::Restructured::DOM";
}

# Plug-in handler for system directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub main {
    my($parser, $name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = Text::Restructured::Directive::parse_directive
	($parser, $dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq $DOM;
    my($args, $options) = map($dhash->{$_}, qw(args options));

    return Text::Restructured::Directive::system_msg
	($parser, $name, 3, $source, $lineno, "Argument(s) required.", $lit)
	if $args =~ /^$/;

    if (! $Perl::safe) {
	# Create a safe compartment for the Perl to run
	use Safe;
	$Perl::safe = new Safe "Perl::Safe";
	# Grant privileges to the safe if -D trusted specified
	$Perl::safe->mask(Safe::empty_opset()) if $main::opt_D{trusted};
	# Share $opt_ variables, $^A to $^Z, %ENV, STDIN, STDOUT, STDERR,
	# VERSION
	my @vars = grep(/^opt_|^[\x00-\x1f]|^(ENV|STD(IN|OUT|ERR)|VERSION)\Z/,
			keys %main::);
	foreach (@vars) {
	    local *var = $main::{$_};
	    *{"Perl::Safe::$_"} = *var;
	}
	# Share RST and DOM subroutines
	foreach (keys %Text::Restructured::) {
	    local *opt = $Text::Restructured::{$_};
	    no strict 'refs';
	    *{"Perl::Safe::Text::Restructured::$_"} =
		\&{"Text::Restructured::$_"}
	    if defined &{"Text::Restructured::$_"};
	}
	foreach (keys %Text::Restructured::DOM::) {
	    local *opt = $Text::Restructured::DOM::{$_};
	    no strict 'refs';
	    *{"Perl::Safe::Text::Restructured::DOM::$_"} =
		\&{"Text::Restructured::DOM::$_"}
	    if defined &{"Text::Restructured::DOM::$_"};
	}
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
    return $parser->system_message(4, $source, $lineno,
			       qq(Error executing "$name" directive: $err.),
			       $lit)
	if $@;

    return Text::Restructured::Directive::system_msg
    ($parser, $name, 3, $source, $lineno, "Non-zero exit code: " .
     ($errmsg || $text || '') )
    if $syserr && ! defined $options->{lenient};
    my $newsource = qq($name directive at $source, line $lineno);
    $text .= $errmsg if defined $errmsg;
    if (defined $options->{literal} && $text ne '') {
	my $lb = $DOM->new('literal_block', %Text::Restructured::XML_SPACE,
			 source=>$newsource);
	$lb->append($DOM->newPCDATA($text));
	return $lb;
    }
    elsif ($parent->{tag} eq 'substitution_definition') {
	chomp $text;
	$parent->append($DOM->newPCDATA($text));
    }
    else {
	$parser->Paragraphs($parent, $text, $newsource, 1) if defined $text;
    }

    return;
}

1;
