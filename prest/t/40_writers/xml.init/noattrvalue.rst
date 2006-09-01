Check xml output for attribute with no value.

.. perl::
   my $dom = new Text::Restructured::DOM('nosuchtag', 'undefattr', undef);
   return $dom;
