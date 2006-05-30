If Test
=======

Run with trusted=1.

Make sure ``perl`` directive can't take privileges away from ``if``.

.. perl:: $main::opt_D{trusted} = 0; "";

Trusted is not required for safe operations like the following.

.. if:: $main::opt_w eq 'dom'

   This text should appear, since we're using the 'dom' writer.

However, it is required for things like opening a file.

.. if:: $main::opt_D{trusted} = 0; open F,"trusted02.rst"

   This text *should* appear, since we should have privileges to run
   unsafe operations.

Make sure we can't take away privileges in previous ``if`` directive.

.. if:: open F,"trusted02.rst"

   This text *should* also appear.

A paragraph.
