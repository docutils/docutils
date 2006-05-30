If Test
=======

Run with trusted=0.

Make sure ``perl`` directive can't give privileges to ``if``.

.. perl:: $main::opt_D{trusted} = 1; "";

Trusted is not required for safe operations like the following.

.. if:: $main::opt_w eq 'dom'

   This text should appear, since we're using the 'dom' writer.

However, it is required for things like opening a file.

.. if:: $main::opt_D{trusted} = 1; open F,"include1.txt"

   This text should *not* appear, since it tries an unsafe operation.

Make sure we can't give ourselves privileges in previous ``if`` directive.

.. if:: open F,"include1.txt"

   This text should also *not* appear.

A paragraph.
