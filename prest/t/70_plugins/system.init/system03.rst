System Test
===========

.. perl::
   # Remove insecure path for -T
   $ENV{PATH} = '';

The following system directive should generate a system error.

.. perl::

   << "EOS"
   .. system:: $^X -e 'die "Errare humanum est"'
      :literal:
   EOS

The following system directive should include the literal text of an
error message.

.. perl::

   << "EOS"
   .. system:: $^X -e 'die "Errare humanum est"'
      :literal:
      :lenient:
   EOS

A paragraph.
