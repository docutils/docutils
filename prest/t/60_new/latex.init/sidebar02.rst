.. perl::

   my $DOM = "Text::Restructured::DOM";
   my $dom = $DOM->new('sidebar');
   $dom->append($DOM->new('paragraph')
                ->append($DOM->newPCDATA('Sidebar with no title')));
