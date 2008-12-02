.. perl::

   my $dom = Text::Restructured::DOM->new('literal');
   $dom->{attr}{test} = "\x0b\x80\x{0110}";
   $dom;
