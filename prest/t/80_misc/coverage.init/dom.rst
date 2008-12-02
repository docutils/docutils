.. perl::
   my $dom = Text::Restructured::DOM->new('literal_block');
   die unless $dom->index($dom) == -1;
   my $child = Text::Restructured::DOM->new('literal');
   $dom->append($child);
   die if defined $child->next();
   die if defined $child->last();
   eval { $dom->Recurse( sub { die }, 'pre' ) };
   eval { $dom->Recurse( sub { die }, 'post' ) };
   eval { $dom->Reshape( sub { die }, 'pre' ) };
   eval { $dom->Reshape( sub { die }, 'post' ) };
   my %tests =
      ('$dom->is_body_elt'         => 1,
       '$dom->is_inline_elt'       => 0,
       '$dom->takes_inline_elts'   => 1,
       '$child->is_body_elt'       => 0,
       '$child->is_inline_elt'     => 1,
       '$child->takes_inline_elts' => 0,
       );
   foreach (sort keys %tests) {
       die "Failed $_: $@" if eval($_) != $tests{$_} || $@;
   }
   # Test creation of DOM with null tag
   my $null_dom = Text::Restructured::DOM->new();
   # Test substitute when I have no parent
   $null_dom->substitute();
   # Test substitute when I'm not in my parent
   my $child = Text::Restructured::DOM->new('child');
   $null_dom->append($child);
   @{$null_dom->{content}} = ();  # Don't try this at home, kiddies
   $child->substitute();
   return "Test was successful."
