System Test
===========

.. perl::
   # Remove insecure path for -T
   $ENV{PATH} = '';

The following system directive should generate a system message.

.. perl::

   << "EOS"
   .. system:: $^X -e 'die "Errare humanum est"'
      :literal:
   EOS

A paragraph.
