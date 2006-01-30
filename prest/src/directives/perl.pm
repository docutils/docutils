# $Id: perl.pm 768 2006-01-28 03:33:28Z marknodine $
# Copyright (C) 2002-2005 Freescale Semiconductor, Inc.
# Distributed under terms of the GNU General Public License (GPL).

# This package implements the perl directive for the perl implementation
# of reStructuredText.

=pod
=begin reST
=begin Description
Executes perl code and interpolates the results.  The code can be
contained either in the arguments or the contents section (or
both). It has the following options:

``:lenient:``
  Causes the exit code for the subprocess to be ignored.
``:literal:``
  Interpret the returned value as a literal block.

If this option is not present, the return value is interpreted
based on its type.  If you return a text string, the text is
interpreted as reStructuredText and is parsed again.  If you
return an internal DOM object (or list of them), the object is
included directly into the parsed DOM structure.  (This latter
option requires knowledge of trip internals, but is the only way
to create a pending DOM object for execution at transformation
time rather than parse time.)

The perl directive defines the following global variables:

``$SOURCE``
   The name of the source file containing the perl directive.
``$LINENO``
   The line number of the perl directive within $SOURCE.
``$opt_<x>``
   The ``<x>`` option from the command line.
``$TOP_FILE``
   The name of the top-level file.
``$VERSION``
   The version of trip.

The following defines are processed by the perl directive:

-D perl='perl-code'
                Specifies some perl code that is executed prior
                to evaluating the first perl directive.  This
                option can be used to specify variables on the
                command line; for example::

                  -D perl='$a=1; $b=2'

                defines constants ``$a`` and ``$b`` that can
                be used in a perl block.
-D trusted      Must be specified for perl directives to use any
                operators normally masked out in a Safe environment.
                This requirement is to prevent a perl directive in a
                file written elsewhere from doing destructive things
                on your computer.
=end Description
=end reST
=cut

package RST::Directive::perl;

BEGIN {
    RST::Directive::handle_directive('perl', \&RST::Directive::perl::main);
}

# Plug-in handler for perl directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub main {
    my($name, $parent, $source, $lineno, $dtext, $lit) = @_;
    print STDERR "Debug: $name: $source, $lineno\n" if $main::opt_d >= 3;
    my $dhash = RST::Directive::parse_directive($dtext, $lit, $source, $lineno);
    return $dhash if ref($dhash) eq 'DOM';
    my($args, $options, $content) = map($dhash->{$_}, qw(args options content));
    return RST::Directive::system_message($name, 3, $source, $lineno,
					  qq(Cannot have both argument and content.),
					  $lit)
	if $args !~ /^$/ && $content !~ /^$/;
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
    
    if (defined $main::opt_D{perl}) {
	my $exp = $main::opt_D{perl};
	$Perl::safe->reval($exp);
	delete $main::opt_D{perl};
	my $err = $@ =~ /trapped by/ ? "$@Run with -D trusted if you believe the code is safe" : $@;
	return RST::system_message(4, $source, $lineno,
				   qq(Error executing "-D perl" option: $err.),
				   $exp)
	    if $@;
    }
    $Perl::Safe::SOURCE = $source;
    $Perl::Safe::LINENO = $lineno;
    $Perl::Safe::TOP_FILE = $main::TOP_FILE;
    my @text = $Perl::safe->reval("$args$content");
    my $newsource = qq($name directive at $source, line $lineno);
    my $err = $@ =~ /trapped by/ ? "$@Run with -D trusted if you believe the code is safe" : $@;
    return RST::system_message(4, $source, $lineno,
			       qq(Error executing "$name" directive: $err.),
			       $lit)
	if $@ && ! defined $options->{lenient};
    push @text, $@ if $@;
    if (defined $options->{literal}) {
	my $text = join('',@text);
	if ($text !~ /^$/) {
	    my $lb = new DOM('literal_block', %RST::XML_SPACE,
			     source=>$newsource);
	    $lb->append(newPCDATA DOM($text));
	    return $lb;
	}
    }
    else {
	my $text;
	if ($parent->{tag} eq 'substitution_definition') {
	    my @doms;
	    if (@text == 0) { }
	    elsif (@text == 1) {
		my $fake = new DOM('fake');
		RST::Paragraphs($fake, $text[0], $newsource, 1);
		my $last = $fake->last();
		if (@{$fake->{content}} == 1 && $last->{tag} eq 'paragraph') {
		    return () unless @{$last->{content}};
		    chomp $last->{content}[-1]{text}
		    if defined $last->{content}[-1]{text};
		    return  @{$last->{content}};
		}
		push(@doms, grep($_->{tag} eq 'system_message' && do {
		    delete $_->{attr}{backrefs}; 1},
				 @{$fake->{content}}));
	    }
	    else {
		push @doms, RST::system_message(3, $source, $lineno,
						qq(Error in "$name" directive within substitution definition: may contain a single paragraph only.));
	    }
	    return @doms;
	}
	else {
	    my $unprocessed = '';
	    foreach $text (@text) {
		next unless defined $text;
		if (ref($text) =~ /\bDOM$/) {
		    &RST::Paragraphs($parent, "$unprocessed\n", $newsource, 1)
			if $unprocessed ne '';
		    $unprocessed = '';
		    # Convert any internal transform reference to point
		    # within the safe
		    $text->{internal}{'.transform'} =
			"Perl.Safe.$text->{internal}{'.transform'}"
			if (defined $text->{internal} &&
			    defined $text->{internal}{'.transform'});
		    $parent->append($text);
		}
		elsif ($parent->{tag} eq 'substitution_definition') {
		    chomp $text;
		    $parent->append(newPCDATA DOM($text));
		}
		else {
		    $unprocessed .= $text;
		}
	    }
	    &RST::Paragraphs($parent, "$unprocessed\n", $newsource, 1)
		if $unprocessed ne '';
	}
    }

    return;
}

1;
