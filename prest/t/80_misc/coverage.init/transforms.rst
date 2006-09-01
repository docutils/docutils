A pending DOM whose transform routine is not defined.

.. perl::
   my $dom = new Text::Restructured::DOM('pending');
   $dom->{'source'} = $SOURCE;
   $dom->{'lineno'} = $LINENO;
   $dom->{'internal'}{'.transform'} = 'test.nosuchtransform';
   return $dom;


A pending DOM whose transform routine has a run-time error.

.. perl::
   my $dom = new Text::Restructured::DOM('pending');
   $dom->{'source'} = $SOURCE;
   $dom->{'lineno'} = $LINENO;
   $dom->{'internal'}{'.transform'} = 'test.badtransform';
   return $dom;
   sub test::badtransform { die "Died\n" }
