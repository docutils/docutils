.. perl::

   my $DOM = 'Text::Restructured::DOM';
   my $dom = $DOM->new('enumerated_list', enumtype=>'arabic');
   $dom->append($DOM->new('list_item')
                ->append($DOM->new('paragraph')
                         ->append($DOM->newPCDATA
                                  ('Enumerated list with no prefix or suffix')
                                 )));
   $dom;
