System Test
===========

.. perl:: # Remove insecure path for -T
   use PrestConfig;
   my $perl_dir = $1 if $PrestConfig::SAFE_PERL =~ m|(.*)/perl$|;
   $ENV{PATH} = "/bin:$perl_dir"; '';

The following system directive should generate a system message.

.. system:: perl -e 'die "Errare humanum est"'
   :literal:

A paragraph.
