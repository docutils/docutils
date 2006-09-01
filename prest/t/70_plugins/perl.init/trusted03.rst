Check for unsafe operations when using ``-D perl='expression'``.
The following should fail because of such errors.  Run with -D trusted=0.

.. perl:: $a ? 'This text should not appear.' : ''

A paragraph.
