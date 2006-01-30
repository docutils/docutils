System Test
===========

Run with trusted=0.

Make sure ``perl`` directive can't give privileges to ``system``.

.. perl:: $main::opt_D{trusted} = 1; 
   $ENV{PATH} = '/bin'; ""; # Remove insecure path for -T

Trusted is required for any system operation.

.. system:: cat include1.txt

A paragraph.
