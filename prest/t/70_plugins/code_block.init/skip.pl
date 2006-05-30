# Make sure states can be run
my @path = split /:/, $ENV{PATH};
my ($states) = grep -x "$_/states", @path;
die "no states executable on path" unless $states;

my $subdir = "Text/Restructured/Directive";
# Make sure our pm file is available
my ($pm) = grep -f "$_/$subdir/code_block.pm", @INC;
die "can't locate $subdir/code_block.pm in \@INC (\@INC contains @INC)" 
    unless $pm;

# Make sure our rst.st file is available
my ($rst_st) = grep -f "$_/$subdir/rst.st", @INC;
die "can't locate states configuration file $subdir/rst.st in \@INC (\@INC contains @INC)" 
    unless $rst_st;
