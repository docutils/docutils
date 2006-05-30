# Make sure our pm file is available
my $subdir = "Text/Restructured/Directive";
my ($pm) = grep -f "$_/$subdir/perl.pm", @INC;
die "can't locate $subdir/perl.pm in \@INC (\@INC contains @INC)" 
    unless $pm;
