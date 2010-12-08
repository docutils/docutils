# $Id$
# Copyright (C) 2002-2005 Freescale Semiconductor, Inc.
# Distributed under terms of the Perl license, which is the disjunction of
# the GNU General Public License (GPL) and the Artistic License.

# This package implements the perl directive for the perl implementation
# of reStructuredText.

package Text::Restructured::Directive::perl;

($VERSION) = q$Revision$ =~ /(\d+)/g;

=pod
=begin reST
=begin Description
Executes perl code and interpolates the results.  The code can be
contained either in the arguments or the contents section (or
both). It has the following options:

``:lenient:``
  Causes the exit code for the subprocess to be ignored.
``:file: <filename>``
  Takes the perl code from file <filename>.
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

The perl directive makes the following global variables available for
use within the perl code:

``$SOURCE``
   The name of the source file containing the perl directive.
``$LINENO``
   The line number of the perl directive within $SOURCE.
``$DIRECTIVE``
   The literal text of the perl directive.
``$SUBSTITUTION``
   The name of a substitution definition being defined by the perl
   directive, or undefined if not within a substitution definition
``@INCLUDES``
   Array of [filename, linenumber] pairs of files which have included this one.
``$opt_<x>`` or ``$opt{x}``
   The ``<x>`` option from the command line.  Changing one of these
   variables has no effect upon the parser.  However, you can
   effectively set the ``-D x=y`` option, possibly affecting
   subsequent parsing, by assigning ``y`` to ``$PARSER->{opt}{D}{x}``.
``$PARSER``
   The Text::Restructured parser object to allow text parsing within a
   perl directive.
``$TOP_FILE``
   The name of the top-level file.
``$VERSION``
   The version of prest (${main::VERSION}).

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

use vars qw($DOM);
BEGIN {
    *DOM = "Text::Restructured::DOM";
}

# Plug-in handler for perl role
# Arguments: parser object, source, lineno
sub init {
    my ($parser, $source, $lineno) = @_;

    # Define the perl directive
    Text::Restructured::Directive::handle_directive
	(perl => \&Text::Restructured::Directive::perl::directive);
    # Define the perl role
    $parser->DefineRole(perl    => undef,
			text    => \&Text::Restructured::Directive::perl::role,
			options => { reparse => 1 },
			);
    create_safe($parser, $source, $lineno);
}

# Plug-in handler for perl directives.
# Arguments: parser object, directive name, parent, source, line number,
#            directive text, literal text
# Returns: array of DOM objects
sub directive {
    my($parser, $name, $parent, $source, $lineno, $dtext, $lit) = @_;
    print STDERR "Debug: $name: $source, $lineno\n" if $parser->{opt}{d} >= 3;
    my @optlist = qw(file lenient literal);
    my $dhash = Text::Restructured::Directive::parse_directive
	($parser, $dtext, $lit, $source, $lineno, \@optlist);
    return $dhash if ref($dhash) eq $DOM;
    my($args, $options, $content) = map($dhash->{$_}, qw(args options content));
    return Text::Restructured::Directive::system_msg
	($parser, $name, 3, $source, $lineno,
	 qq(Cannot have both argument and content.), $lit)
	if $args !~ /^$/ && $content !~ /^$/;
    my $code = "$args$content";
    if ($options->{file}) {
	return Text::Restructured::Directive::system_msg
	    ($parser, $name, 3, $source, $lineno,
	     qq(Cannot have both :file: and content.), $lit)
	    if $code ne '';
	open FILE, $options->{file} or
	    return Text::Restructured::Directive::system_msg
	    ($parser, $name, 3, $source, $lineno,
	     qq(Cannot open file "$options->{file}".), $lit);
	$code = join '', <FILE>;
	close FILE;
    }

    my $subst = $parent->tag eq 'substitution_definition' ?
    $parent->{attr}{names}[0] : undef;

    my @text = evaluate_code($parser, $code, $source, $lineno, $lit, $subst);
    my $err = $@ =~ /trapped by/ ?
	"$@Run with -D trusted if you believe the code is safe." : $@;
    chomp $err;
    return $parser->system_message
	(4, $source, $lineno,
	 qq(Error executing "$name" directive: $err), $lit)
	if $@ && ! defined $options->{lenient};

    push @text, $@ if $@;
    my $newsource = qq($name directive at $source, line $lineno);

    if (defined $options->{literal}) {
	my $text = join('',@text);
	if ($text !~ /^$/) {
	    my $lb = $DOM->new('literal_block', %Text::Restructured::XML_SPACE,
			       source=>$newsource);
	    $lb->append($DOM->newPCDATA($text));
	    return $lb;
	}
    }
    else {
	my $text;
	if ($parent->tag eq 'substitution_definition') {
	    my @doms;
	    if (@text == 0) { }
	    elsif (@text == 1) {
		my $fake = $DOM->new('fake');
		$parser->Paragraphs($fake, $text[0], $newsource, 1);
		my $last = $fake->last;
		if ($fake->contents == 1 && $last->tag eq 'paragraph') {
		    chomp $last->last->{text}
		    # uncoverable branch true note:paragraph always has #PCDATA
		    # uncoverable branch true note:paragraph always has #PCDATA
		    if defined $last->last->{text};
		    return  $last->contents;
		}
		push(@doms, grep($_->tag eq 'system_message' && do {
		    delete $_->{attr}{backrefs}; 1}, $fake->contents));
	    }
	    else {
		push @doms, $parser->system_message(3, $source, $lineno,
						    qq(Error in "$name" directive within substitution definition: may contain a single paragraph only.),
						    $lit);
	    }
	    return @doms;
	}
	else {
	    foreach $text (@text) {
		next unless defined $text;
		if (ref($text) =~ /$DOM$/o) {
		    # Convert any internal transform reference to point
		    # within the safe
		    $text->{internal}{'.transform'} =
			"Perl.Safe.$text->{internal}{'.transform'}"
			if (defined $text->{internal} &&
			    defined $text->{internal}{'.transform'});
		    $parent->append($text);
		}
		else {
		    $parser->Paragraphs($parent, "$text\n", $newsource, 1)
			if $text ne '';
		}
	    }
	}
    }

    return;
}

# Plug-in handler for perl role text.
# Arguments: parser object, text, role name, source, line number,
# Returns: array of DOM objects
sub role {
    my ($parser, $code, $role, $source, $lineno) = @_;

    my @text = evaluate_code($parser, $code, $source, $lineno);
    my $err = $@ =~ /trapped by/ ?
	"$@Run with -D trusted if you believe the code is safe." : $@;
    return [$parser->system_message
	    (4, $source, $lineno,
	     qq(Error executing "$role" role: $err), $lit)]
	    if $@;
    return \@text;
}

# Creates the perl Safe area to execute code in and initializes it with
# any -D perl arguments.
# Arguments: Text::Restructured object
# Returns: None
# Sets globals: $Perl::safe
sub create_safe {
    my ($parser, $source, $lineno) = @_;

    if (! $Perl::safe_world) {
	# Create a safe world to run the Perl code in
	use Safe::World;
	$Perl::safe_world = Safe::World->new(flush=>1, root=>'Perl::Safe');
	$Perl::safe_world->block_stderr;
	# Grant privileges to the safe if -D trusted is specified
	$Perl::safe_world->op_permit_only(':default');
	$Perl::safe_world->op_deny_only() if $parser->{opt}{D}{trusted};
	# Share $opt_ variables, $^A to $^R, $^T to $^Z, %ENV, VERSION
	$Perl::safe_world->share_vars('main' => ['%ENV']);
	my @vars = grep(/^[\x00-\x12\x14-\x1f]\Z|^(VERSION)\Z/,
			keys %main::);
	foreach (@vars) {
	    $Perl::safe_world->set("\$$_", ${$_});
	}
	# Share $opt_ variables
 	foreach (keys %{$parser->{opt}}) {
	    my $opt = $parser->{opt}{$_};
	    # uncoverable branch true count:2 note:Currently no array opts
	    my $type =
		ref $opt eq 'ARRAY' ? '@' : ref $opt eq 'HASH' ? '%' : '$';
	    $Perl::safe_world->set("${type}opt_$_", $opt);
 	}
	# Share RST and DOM subroutines
	foreach my $pm (qw(Text::Restructured Text::Restructured::DOM)) {
	    no strict 'refs';
	    foreach (keys %{"${pm}::"}) {
		local *opt = ${"${pm}::"}{$_};
		*{"Perl::Safe::${pm}::$_"} = \*{"${pm}::$_"};
	    }
	}
	if (defined $parser->{opt}{D}{perl}) {
	    my $exp = $parser->{opt}{D}{perl};
	    $Perl::safe_world->eval($exp);
	    delete $parser->{opt}{D}{perl};
	    my $err = $@ =~ /trapped by/ ? "$@Run with -D trusted if you believe the code is safe." : $@;
	    return $parser->system_message
		(4, $source, $lineno,
		 qq(Error executing "-D perl" option: $err), $exp)
		if $@;
	}
    }

    return;
}

# Evaluates a code string within the Perl Safe box
# Arguments: Text::Restructured object, code string,
#            source, line number, literal text (if a directive),
#            substition name (if within a substitution definition)
# Returns: Array of whatever the code returns
# Side-effects: Sets $@
sub evaluate_code {
    my ($parser, $code, $source, $lineno, $lit, $subst) = @_;

    my @text;
    # uncoverable branch true note:Coverage unavailable in safe_world
    if ($main::SAFEWORLD) {
	# We're already in the safe box: just eval
	# uncoverable statement note:Coverage unavailable in safe_world
	local $main::SOURCE    = $source;
	# uncoverable statement note:Coverage unavailable in safe_world
	local $main::LINENO    = $lineno;
	# uncoverable statement note:Coverage unavailable in safe_world
	local $main::DIRECTIVE = $lit;
	# uncoverable statement note:Coverage unavailable in safe_world
	local $main::SUBSTITUTION = $subst;
	# uncoverable statement note:Coverage unavailable in safe_world
	local $main::PARSER    = $parser;
	# uncoverable statement note:Coverage unavailable in safe_world
	return eval "package main; $code";
    }
    else {
	$Perl::safe_world->set_vars
	    ('$SOURCE'    => $source,
	     '$LINENO'    => $lineno,
	     '$DIRECTIVE' => $lit,
	     '$SUBSTITUTION' => $subst,
	     '$TOP_FILE'  => $parser->{TOP_FILE},
	     '$PARSER'    => $parser,
	     '@INCLUDES'  => \@Text::Restructured::INCLUDES);
	return $Perl::safe_world->eval($code);
    }
}

1;
