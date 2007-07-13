.. Tests a perl directive within a perl directive

.. perl::
   my $dom = Text::Restructured::DOM->new('fake');
   $main::PARSER->Paragraphs($dom, '.. perl:: "We have a winner!\n"',
			     $main::SOURCE, $main::LINENO);
   $dom->contents;
