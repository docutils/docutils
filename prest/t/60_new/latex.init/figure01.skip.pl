use Text::Restructured::PrestConfig;
die "cannot run with tainting (-T)\n"
    unless $Text::Restructured::PrestConfig::TAINT eq 'No';
my @path = split /:/, $ENV{PATH};
my ($convert) = grep -x "$_/convert", @path;
die "cannot find convert program\n"
    unless $convert;
