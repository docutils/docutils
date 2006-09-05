package Text::Restructured::Writer;

# $Id: DOM.pm 4580 2006-05-30 22:02:21Z mnodine $
# Copyright (C) 2006 Intrinsity, Inc.
# Distributed under terms of the Perl license, which is the disjunction of
# the GNU General Public License (GPL) and the Artistic License.

# This package contains routines for parsing and processing 
# writer schemas for Text::Restructured.

# Data structures:
#   _`Text::Restructured::Writer`: Hash reference with following keys:
#       ``handler``:  Reference to hash whose keys are phase names and
#                     whose values are handler structures.
#       ``phases``:   Reference to array of phase names (in order)
#   handler: Hash reference with the following keys
#       ``tag``:      Regular expression of DOM tags handled by the handler
#       ``line``:     The file and line number where the handler routine
#                     was defined
#       ``text``:     Text of the subroutine's definition
#       ``code``:     Subroutine reference implementing text

use strict;

# CLASS METHOD.
# Creates a new Writer object.
# Arguments: writer name
# Returns: Writer object
sub new {
    my ($class, $writer_name) = @_;

    my $writer = bless { };
    $writer->ParseSchema($writer_name);
    $writer->Precompile();
    return $writer;
}

# Parses the writer's schema file.
# Arguments: file name
# Returns: None
# Modifies instance variables: phases, handler
sub ParseSchema : method {
    my ($self, $writer) = @_;

    my $file = $writer;
    use vars qw($newfile);
    local $newfile = $file;
    my @dirs = grep(-r "$_/Text/Restructured/Writer/$file.wrt", @INC);
    die "Cannot find schema for writer $writer" unless @dirs;
    $file = "$dirs[0]/Text/Restructured/Writer/$file.wrt";
    no strict 'refs';
    # Devel::Cover branch 0 0 Cannot force open failure
    open $newfile,$file or die "Cannot open writer file $file";

    my %phases;
    my $phase = '';
    my $nest = my $in_sub = 0;
    # Note: Turn warnings off while reading from newfile since it will
    # cause a "read of closed filehandle" warning with -w.
    while (do { local $^W=0; $_ = <$newfile> }) {
	if ($nest <= 1 && ! $in_sub) {
	    next if /^=pod/ .. /^=cut/;
	    next if /^\s*$/ || /^\s*\#/;
	    if (/^\s*(?:(phase|sub)\s+)?(\S+)\s*(=\s*)?\{\s*(?:\#.*)?$/i) {
		if ($nest == 0 && $1 eq 'phase') {
		    $phase = $2;
		    push @{$self->{phases}}, $phase unless $phases{$phase}++;
		}
		else {
		    my $tag = $2;
		    $tag =~ s/(\()(?!\?)/$1?:/g;
		    push(@{$self->{handler}{$phase}},
			 {tag=>$tag, line=>"$file, line $."});
		    $in_sub = $nest+1;
		}
		$nest++;
	    }
	    elsif (/^\s*\}\s*$/) {
		$nest--;
	    }
	    else {
		die "$file:$.: Parse error: $_";
	    }
	}
	else {
	    my $left = y/\{/\{/;
	    my $right = y/\}/\}/;
	    $nest += ($left - $right);
	    $self->{handler}{$phase}[-1]{text} .= $_ if $nest >= $in_sub;
	    $in_sub = 0 if $nest < $in_sub;
	}
	die "Unmatched } in schema file $writer.wrt" if $nest < 0;
	# Make sure $. is relative to the current file
	close $newfile if eof;
    }
    die "Unmatched { in schema file $writer.wrt" if $nest > 0;
    close $newfile;
}

# Precompiles the writer's subroutines
# Arguments: None
# Returns: None
# Modifies instance variables: handler
sub Precompile : method {
    my ($self) = @_;
    # Precompile the handler routines
    my $phase;
    foreach $phase (sort keys %{$self->{handler}}) {
	my $handler;
	foreach $handler (@{$self->{handler}{$phase}}) {
	    # Need to untaint the text for the subroutine.
	    ($handler->{text} || '') =~ /(.*)/s;
	    my $text = $1;
	    $handler->{code} = DoEval($text, $handler->{line},
				      $phase eq '' ? $handler->{tag} : undef);
	}
    }
}

# Passes the DOM through all phases of the writer and returns the
# output string.
# Arguments: parsed DOM
# Returns: string
sub ProcessDOM : method {
    my ($self, $dom) = @_;
    my $str = '';
    foreach my $phase (@{$self->{phases}}) {
	$str .= $self->ProcessDOMPhase($dom, $phase);
    }
    return $str;
}

# Passes the DOM through a specific phase of the writer and returns 
# the output string.  Uses the current phase if no phase is specified.
# Arguments: parsed DOM, phase name
# Returns: string returned from processing the phase
sub ProcessDOMPhase : method {
    my ($self, $dom, $phase) = @_;
    my $handarray = $self->{handler}{$phase};
    my $searchstring = "^(?:" . join('|',map("($_->{tag})",@$handarray)) .
	')$';
    my $str = $self->TraverseDOM($dom, $phase, $handarray, $searchstring);
    return defined $str ? $str : '';
}

# Internal routine called by TraverseDOM to do recursive handling of DOM tree.
# Arguments: parsed DOM, handler array reference, search string
sub TraverseDOM : method {
    my ($self, $dom, $phase, $handarray, $searchstring) = @_;
    my @matches = $dom->{tag} =~ /$searchstring/;
    my @match = grep(defined $matches[$_], (0 .. $#{$handarray}));
    my $match = $match[0];
    my $str;

    foreach my $content (@{$dom->{content}}) {
	my $val = $self->TraverseDOM($content, $phase, $handarray,
				     $searchstring);
	$content->{val} = $val;
    }
    my $substr = join('',map(defined $_->{val} ? $_->{val} : '',
			     @{$dom->{content}}));
    if (defined $match) {
	print STDERR "$phase: $dom->{tag}\n" if $main::opt_d >= 1;
	$str = eval { &{$handarray->[$match]{code}}
		      ($dom, $substr, $self, $phase) };
	print STDERR "$str\n"
	    if $main::opt_d >= 2 && defined $str && $str ne '';
	die "Error: $handarray->[$match]{line}: $@" if $@;
    }

    return $str;
}

# Precompiles a subroutine that evaluates an expression.
# Arguments: string expression, line number, optional subroutine name
# Returns: anonymous subroutine reference
# Exceptions: Program termination if error in evaluation
# Uses globals: None
# Sets globals: ``Eval_::<subname>``
sub DoEval {
    my ($str, $line, $subname) = @_;
    my ($file, $lineno) = $line =~ /(.*), line (\d+)/;
    print STDERR "$line\n" if $main::opt_d >= 1;
    # N.B. Don't just set to $line because it may be tainted
    if (! $subname) {
	my ($f) = $file =~ m!([^/]+)$!;
	$subname = "$f, line $lineno";
    }
    $subname =~ s/\W/_/g;
    my $sub = "sub Eval_::$subname {package Eval_;\n $str}";
    my $line_directive = defined $main::opt_D{'no_line_directives'} ? "" :
	qq(\#line $lineno "$file"\n);
    my $val = eval("$line_directive$sub");
    die "Error: $line: $@" if $@;
    return \&{$Eval_::{$subname}};
}

1;
