Call a pending transform routine with -d.

.. perl::
   $DOM = 'Text::Restructured::DOM';
   my $dom = $DOM->new('pending');
   $dom->{'source'} = $SOURCE;
   $dom->{'lineno'} = $LINENO;
   $dom->{'internal'}{'.transform'} = 'test.mytransform';
   return $dom;
   sub test::mytransform {
       return $DOM->newPCDATA("You called test::mytransform.");
   }
