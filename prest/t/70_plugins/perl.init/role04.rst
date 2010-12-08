.. perl::

   sub ParentClass {
       my ($tag, $class) = @_;
       ".. class:: $class\n" .
       "   :parent: $tag\n";
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
