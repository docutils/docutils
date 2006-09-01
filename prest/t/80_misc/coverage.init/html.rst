Checks that the writer issues a warning if encountering an unknown DOM
tag.

.. perl::
   $DOM = 'Text::Restructured::DOM';
   my $dom = $DOM->new('nosuchtag', 'undefattr', undef);
   $dom->{_html}{attr}{undefattr} = undef;
   return $dom;

Checks that the writer behaves OK if a comment has something besides #PCDATA.

.. perl::

   my $dom = $DOM->new('comment');
   $dom->append($DOM->newPCDATA('A comment'));
   my $bq = $DOM->new('block_quote');
   $dom->append($bq);
   $bq->append($DOM->newPCDATA('A block quote in a comment'));
   return $dom;


