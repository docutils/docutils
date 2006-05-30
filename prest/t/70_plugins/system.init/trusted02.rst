System Test
===========

Run with trusted=1.

Make sure ``perl`` directive can't take away privileges from ``system``.

.. perl:: $main::opt_D{trusted} = 0; 
   $ENV{PATH} = '/bin'; ""; # Remove insecure path for -T

Trusted is required for any system operation.

.. system:: cat ../data/include1.txt

A paragraph.
