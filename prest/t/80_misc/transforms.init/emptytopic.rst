Test an empty topic.

.. perl::

   my $DOM = 'Text::Restructured::DOM';
   my $dom = $DOM->new('topic');
   $dom;

.. perl::

   my $DOM = 'Text::Restructured::DOM';
   my $dom = $DOM->new('topic');
   $dom->append($DOM->new('title')
                ->append($DOM->newPCDATA('Title but no content')));
   $dom;
