.. perl::

   my $dom = Text::Restructured::DOM->new('topic', classes=>['unsupported']);
   $dom->{title} = 'Title';
   $dom->append(Text::Restructured::DOM->newPCDATA("Unsupported topic.\n"));
   $dom;
