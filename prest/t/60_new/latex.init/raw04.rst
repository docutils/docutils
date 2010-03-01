.. perl::

   my $DOM = 'Text::Restructured::DOM';
   my $dom = $DOM->new('raw',
                       format  => 'latex',
                       refuri  => 'http://latex.org');
   $dom->append($DOM->newPCDATA('Raw \LaTeX with a URL'));
   $dom;
