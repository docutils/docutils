.. A comment with a newline
.. perl::

   my $dom =Text::Restructured::DOM->new('comment');
   $dom->append(Text::Restructured::DOM->newPCDATA('Comment without newline'));
   $dom;
.. Another comment
