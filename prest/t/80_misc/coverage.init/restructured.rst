Pick up internal coverage points in Restructured.pm using perl directives.

Check out DeepCopy.

.. perl::
   my $s_p = \"string";
   my $new_s_p = Text::Restructured::DeepCopy($s_p);
   die if "$s_p" eq "$new_s_p" || $$s_p ne $$new_s_p;
   return "Test was successful."

Call problematic with no id argument.

.. perl::
   my $dom = new Text::Restructured::DOM('pending');
   $dom->{'source'} = $SOURCE;
   $dom->{'lineno'} = $LINENO;
   $dom->{'internal'}{'.transform'} = 'test.mytransform';
   return $dom;
   sub test::mytransform {
       my($dom, $parser, $details) = @_;
       my ($prob) = $parser->problematic("Test problematic", "xid1", "xid2");
       my $id = $parser->NormalizeId();
       die if $id !~ /^id\d+$/;
       $id = $parser->NormalizeName();
       die if defined $id;
       die if $parser->QuoteSimpleTables() ne '';
       return $prob;
   }
