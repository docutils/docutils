.. perl::

   my $DOM = 'Text::Restructured::DOM';
   my $dom = $DOM->new('note');
   $dom->append($DOM->newPCDATA('Note without a newline'));
   $dom;
