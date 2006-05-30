# Make sure our pm file is available
my $subdir = "Text/Restructured/Directive";
my ($pm) = grep -f "$_/$subdir/if.pm", @INC;
die "can't locate $subdir/if.pm in \@INC (\@INC contains @INC)" 
    unless $pm;
