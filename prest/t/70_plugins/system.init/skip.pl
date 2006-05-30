# Make sure our pm file is available
my $subdir = "Text/Restructured/Directive";
my ($pm) = grep -f "$_/$subdir/system.pm", @INC;
die "can't locate $subdir/system.pm in \@INC (\@INC contains @INC)" 
    unless $pm;
