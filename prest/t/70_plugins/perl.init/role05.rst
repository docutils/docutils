Examples that do not strictly meet the DTD.

.. perl::

   sub Yes {
       return "Yes\n\n" .
       "* Put a bullet list here";
   }

   sub No {
       return "No",  $PARSER->system_message(1, $SOURCE, $LINENO,
       "Informational message only", $LITERAL);
   }
   sub New {
       my $para = Text::Restructured::DOM->new('paragraph');
       $para->append(Text::Restructured::DOM->newPCDATA('Another list'));
       (my $item = Text::Restructured::DOM->new('list_item'))->append($para);
       (my $list = Text::Restructured::DOM->new('bullet_list'))->append($item);
       return "New", $list, "*here*";
   }
.. default-role:: perl

=============================== =========== ======== ========
Feature                          Required?   Plan A   Plan B
=============================== =========== ======== ========
Generates `New` Keys             `Yes`        `No`    `New`
=============================== =========== ======== ========
