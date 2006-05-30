This file shows a bug that occurred when parsing line blocks where
lines begin with a substitution reference.

.. |sub1| replace:: Line 1
.. |sub2| replace:: Line 2

| |sub1| here.
| |sub2| here.
