Perl Test
=========

Run with trusted=1.  Make sure we can't take away our privileges.

.. perl:: $main::opt_D{'trusted'} = 0; "";

Trusted is not required for safe operations like the following.

.. perl:: "2*pi is about " . 2*3.1415926535 . "."

However, it is required for things like opening a file.

.. perl:: open F,"../data/include1.txt"; @F = <F>; close F; join('', @F);

A paragraph.
