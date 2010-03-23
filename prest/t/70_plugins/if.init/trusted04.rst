If Test
=======

Run with trusted=1.

Trusted is not required for safe operations like the following.

.. if:: $opt_w eq 'dom'

   This text should appear, since we're using the 'dom' writer.

However, it is required for things like opening a file.

.. if:: open F,"trusted02.rst"

   This text *should* appear, since we should have privileges to run
   unsafe operations.

A paragraph.
