#!/usr/bin/perl -w

# Sort the statements of a dot file produced by `rst2gv` so the sequence of
# lines is stable.

my @gather;

sub add($) {
  my( $ln ) = @_;

  push(@gather, $ln);
}

sub out(@) {
  my( @lns ) = @_;

  print(@lns);
}

sub flush(@) {
  my( @sfx ) = @_;

  out(sort(@gather), @sfx);
  @gather = ( );
}

while(defined($_ = <>)) {
  my $in;
  if($in = /\{\s*$/ .. /\}\s*$/) {
    if($in == 1)
      { out($_); }
    elsif($in =~ /E/)
      { flush($_); }
    elsif(/\{\s*$/)
      { die("Embedded graphs are not supported: $_"); }
    elsif(/\\$/)
      { die("Continuation lines are not supported: $_"); }
    elsif(/\/\*/ || /\/\// || /#/)
      { die("Comments are not supported: $_"); }
    elsif(/;\s*$/) { # A statement
      if(/^\s*(graph|node|edge)/) # An attribute statement
	{ flush($_); }
      # Note: The order of nodes output by `pygraphviz` is random. This changes
      # the semantic of the graph but unfortunately can not be controlled.
      else
	{ add($_); }
    }
    else
      { die("Unsupported line ending: $_"); }
  }
  else
    { out($_); }
}
