.. perl::
   my $dom = new Text::Restructured::DOM();
   die unless $dom->index($dom) == -1;
   my $child = new Text::Restructured::DOM();
   $dom->append($child);
   die if defined $child->next();
   die if defined $child->last();
   eval { $dom->Recurse( sub { die }, 'pre' ) };
   eval { $dom->Recurse( sub { die }, 'post' ) };
   eval { $dom->Reshape( sub { die }, 'pre' ) };
   eval { $dom->Reshape( sub { die }, 'post' ) };
   return "Test was successful."
