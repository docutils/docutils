# $Id: if.pm 729 2005-11-04 22:25:20Z r31609 $
# Copyright (C) 2002-2005 Freescale Semiconductor, Inc.
# Distributed under terms of the Perl license, which is the disjunction of
# the GNU General Public License (GPL) and the Artistic License.

# This package implements the code-block directive for the perl implementation
# of reStructuredText.

=pod
=begin reST
=begin Description
Formats a block of text as a code block.  This directive depends upon
the availability of the "states" program, part of the Unix "enscript"
suite, to mark up the code; otherwise the code block will be returned
as a simple literal block.  The argument is optional and specifies the
source language of the code block.  If the code block is read from a
file, the language will usually default correctly.  The following
language specifications are recognized:

  ada asm awk c changelog cpp elisp fortran haskell html idl java
  javascript mail makefile nroff objc pascal perl postscript python
  scheme sh states synopsys tcl vba verilog vhdl

The directive has the following options:

``:file: <filename>``
  Reads the code sample from a file rather than using the content block.
``:color:``
  Specifies that "color" markup should be done.  What this actually
  means is that the following interpreted-text roles are used for
  parts of the code markup:

  =============== ================
  comment         A comment in the language
  function-name   A function name
  variable-name   A variable name
  keyword         A reserved keyword
  reference-name  A reference name
  string          A quoted string
  builtin         Variable names built into language
  type-name       Names associated with the language's type system
  =============== ================

  If any of these roles is undefined before processing the macro, a
  null definition is entered for them.
``:level: <level>``
  The level of markup.  ``<level>`` can be one of ``none``, ``light``,
  or ``heavy`` (default ``heavy``).  Ignored if ``:color:`` is specified.
``:numbered:``
  Number the lines of the code block.
=end Description
=end reST
=cut

package Text::Restructured::Directive::code_block;

BEGIN {
    Text::Restructured::Directive::handle_directive
	('code_block', \&Text::Restructured::Directive::code_block::main);
}

use vars qw($DOM);
BEGIN {
    *DOM = "Text::Restructured::DOM";
}

# Plug-in handler for code-block directives.
# Arguments: directive name, parent, source, line number, directive text, 
#            literal text
# Returns: array of DOM objects
sub main {
    my($parser, $name, $parent, $source, $lineno, $dtext, $lit) = @_;
    my $dhash = Text::Restructured::Directive::parse_directive
	($parser, $dtext, $lit, $source, $lineno);
    my($args, $content, $content_lineno, $options) =
	map($dhash->{$_} || '', qw(args content content_lineno options));

    return Text::Restructured::Directive::system_msg
	($parser, $name, 3, $source, $lineno,
	 qq(Cannot have both :file: option and content.), $lit)
	if $options->{file} && $content !~ /^$/;

    $ENV{PATH} = "";
    my ($states_bin) = grep -x "$_/states",
	qw(/bin /usr/bin /usr/local/bin);
    my $subdir = "Text/Restructured/Directive";
    my ($st_dir) = grep -r "$_/$subdir/rst.st", @INC;
    my ($st) = grep -r $_, "$st_dir/$subdir/rst.st";

    if ($states_bin && $st) {
	# We have the ability to run states
	my $input_file = $options->{file} || "/tmp/cb_$$";
	my ($src, $src_line) = $options->{file} ? ($input_file, 1)
	    : ($source, $content_lineno);
	if ($content !~ /^$/) {
	    open OUT, ">$input_file";
	    print OUT $content;
	    close OUT;
	}
	my @args = "-D language=rst";
	push @args, "-D colormodel=emacs" if defined $options->{color};
	push @args, "-D hl_level=$options->{level}"
	    if defined $options->{level};
	push @args, "-s $1" if $args =~ /^(\w+)$/;
	my $cmd = "$states_bin/states @args -f $st $input_file 2>&1";
	my $markup = `$cmd`;
	unlink $input_file unless $options->{file};
	my $pl = $DOM->new('parsed_literal', %Text::Restructured::XML_SPACE);
	my @errs;
	if ($?) {
	    $pl->append($DOM->newPCDATA($markup));
	}
	else {
	    $markup = numbered($markup) if defined $options->{numbered};
	    if (defined $options->{color}) {
		# Make sure the roles are defined
		foreach my $role (qw(comment function-name
		                  variable-name keyword reference-name
		                  string builtin type-name)) {
		    $parser->DefineRole($role)
			unless defined $parser->{MY_ROLES}{$role};
		}
	    }
	    @errs = $parser->Inline($pl, $markup, $source, $content_lineno);
	}
	return $pl, @errs;
    }
    else {
	# We can't run states; just create a literal block
	my $lb = $DOM->new('literal_block', %Text::Restructured::XML_SPACE);
	if ($options->{file}) {
	    my $file = $options->{file};
	    $lb->{attr}{source} = $file;
	    if (open(FILE,$file)) {
		my $text = join('',<FILE>);
		$text = numbered($text) if defined $options->{numbered};
		$lb->append($DOM->newPCDATA($text));
	    }
	    else {
		my $err = "IOError: " . system_error();
		return system_message
		    ($parser, 4, $source, $lineno,
		     qq(Problems with "$name" directive path:\n$err: '$args'.),
		     $lit);
	    }
	}
	else {
	    $lb->append($DOM->newPCDATA($content));
	}
	return $lb;
    }

    return;
}

# Numbers the lines of a text block
# Inputs: text block
# Outputs: numbered text 
sub numbered {
    my ($code) = @_;
    my $len = length($code =~ tr/\n//);
    my $c;
    $code =~ s/^/sprintf "%${len}d  ", ++$c/gem;
    $code;
}

1;
