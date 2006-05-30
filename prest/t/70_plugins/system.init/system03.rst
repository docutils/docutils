System Test
===========

.. perl::
   # Remove insecure path for -T
   my $perl_dir = $1 if $^X =~ m|(.*)/|;
   $ENV{PATH} = "$perl_dir:/bin"; '';

The following system directive should generate a system error.

.. system:: perl -e 'die "Errare humanum est"'
   :literal:

The following system directive should include the literal text of an
error message.

.. system:: perl -e 'die "Errare humanum est"'
   :literal:
   :lenient:

A paragraph.
