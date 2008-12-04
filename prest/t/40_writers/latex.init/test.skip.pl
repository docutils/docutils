use Text::Restructured::PrestConfig;
die "cannot run with tainting (-T) prior to version 5.8.6\n"
    unless $Text::Restructured::PrestConfig::TAINT eq 'No' || $] >= 5.008006;
