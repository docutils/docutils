# $Id$
# Copyright (C) 2002-2005 Freescale Semiconductor, Inc.
# Distributed under terms of the Perl license, which is the disjunction of
# the GNU General Public License (GPL) and the Artistic License.

# This package implements the perl directive for the perl implementation
# of reStructuredText.

package Text::Restructured::Directive::if;

($VERSION) = q$Revision$ =~ /(\d+)/g;

=pod
=begin reST
=begin Description
Executes its argument as a perl expression and returns its
content if the perl expression is true.  The content is
interpreted as reStructuredText.  It has no options. It has the same
variables available to it as the ``perl::`` directive.  It processes
the following defines:

-D perl='perl-code'
                Specifies some perl code that is executed prior
                to evaluating the first perl directive.  This
                option can be used to specify variables on the
                command line; for example::

                  -D perl='$a=1; $b=2'

                defines constants ``$a`` and ``$b`` that can
                be used in the perl expression.
-D trusted      Must be specified for if directives to use any
                operators normally masked out in a Safe environment.
                This requirement is to prevent an if directive in a
                file written elsewhere from doing destructive things
                on your computer.
=end Description
=end reST
=cut

use Text::Restructured::Directive::perl;

sub init {
    my ($parser, $source, $lineno) = @_;

    Text::Restructured::Directive::handle_directive
	(if => \&Text::Restructured::Directive::if::main);
    Text::Restructured::Directive::perl::create_safe
	($parser, $source, $lineno);
}

# Plug-in handler for if directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub main {
    my($parser, $name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = Text::Restructured::Directive::parse_directive
	($parser, $dtext, $lit, $source, $lineno, [], '1+');
    return $dhash if ref($dhash) eq "Text::Restructured::DOM";
    my($args, $options, $content) =
	map(defined $dhash->{$_} ? $dhash->{$_} : '',
	    qw(args options content));
    return Text::Restructured::Directive::system_msg
	($parser, $name, 3, $source, $lineno,
	 qq(The $name directive must have content.), $lit)
	if $content =~ /^$/;

    my $val = Text::Restructured::Directive::perl::evaluate_code
        ($parser, $args, $source, $lineno, $lit);
    my $err = $@ =~ /trapped by/ ? "$@Run with -D trusted if you believe the code is safe" : $@;
    return $parser->system_message
	(4, $source, $lineno,
	 qq(Error executing "$name" directive: $err.), $lit)
	if $@;
    return '' unless $val;
    my $newsource = qq($name directive at $source, line $lineno);
    if ($parent->tag eq 'substitution_definition') {
	my @doms;
	my $fake = new Text::Restructured::DOM('fake');
	$parser->Paragraphs($fake, $content, $newsource, 1);
	my $last = $fake->last();
	if ($fake->contents == 1 && $last->tag eq 'paragraph') {
	    chomp $last->last->{text} if $last->last->{text};
	    return $last->contents;
	}
	push @doms, map(do {delete $_->{attr}{backrefs}; $_},
			grep($_->tag eq 'system_message', $fake->contents));
	push @doms, $parser->system_message(3, $source, $lineno,
					    qq(Error in "$name" directive within substitution definition: may contain a single paragraph only.));
	return @doms;
    }
    else {
	$parser->Paragraphs($parent, "$content\n", $newsource, 1);
    }

    return;
}

1;
