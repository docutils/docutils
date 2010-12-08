.. perl::

   sub user::ParentAttr {
      my ($dom, $parser, $details) = @_;
      my $parent = $dom->parent;
      my $tag = $details->{tag};
      while ($parent && $parent->tag ne $details->{tag}) {
	  $parent = $parent->parent;
      }
      return (Text::Restructured::DOM->newPCDATA("**user::ParentAttr could not find parent with tag '$tag'**"))
	  unless $parent;
      $parent->{attr}{$_} = $details->{attr}{$_}
      foreach keys %{$details->{attr}};
      return;
   }

   sub ParentClass {
       my ($tag, $class) = @_;
       my $pending = Text::Restructured::DOM->new('pending');
       $pending->{internal}{'.transform'} = \&user::ParentAttr;
       $pending->{internal}{'.details'} = { attr=> {style=>'font-size:10pt'},
					    tag => $tag
					    };
       $pending->{source} = $SOURCE;
       $pending->{lineno} = $LINENO;
       my $pending2 = Text::Restructured::DOM->new('pending');
       $pending2->{internal}{'.transform'} = 'docutils.transforms.parts.Class';
       $pending2->{internal}{'.details'} = { class  => $class,
                                             parent => 'entry',
					    };
       $pending2->{source} = $SOURCE;
       $pending2->{lineno} = $LINENO;
       return ($pending, $pending2);
   }

   sub Yes {
       return "Yes", ParentClass(entry => 'green');
   }
   sub No {
       return "No",  ParentClass(entry => 'red');
   }
   sub New {
       return "New", ParentClass(entry => 'yellow');
   }

.. default-role:: perl

=============================== =========== ======== ========
Feature                          Required?   Plan A   Plan B
=============================== =========== ======== ========
Plays CDs                        `Yes`       `Yes`    `Yes`
Is satellite-radio compatible    `No`        `Yes`    `No`
Has USB connection for iPod      `No`        `No`     `Yes`
Has iPod input                   `Yes`       `Yes`    `New`
Does Bluetooth hands-free        Helpful     `No`     `Yes`
=============================== =========== ======== ========

.. raw:: html

   <style type="text/css">
   .green  { background-color: lightgreen }
   .red    { background-color: pink }
   .yellow { background-color: yellow }
   </style>
