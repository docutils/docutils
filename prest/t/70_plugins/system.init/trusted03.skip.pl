use Text::Restructured::PrestConfig;
die "cannot run with tainting (-T)\n"
    unless $Text::Restructured::PrestConfig::TAINT eq 'No';
