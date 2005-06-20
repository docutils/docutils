===================
  A pickle writer
===================

:Author: Martin Blais
:Contact: blais@furius.ca
:Date: $Date$


A pickle Writer.

It is difficult at the moment because the Writer class requires us to send the
output as a Unicode string.  Pickle streams contain binary characters (even the
lowest printable protocol contains some 'ascii' undecodable chars.  Even if it
worked, it would not make sense to decode, and then reencode (not efficient).

The Writer must support binary outputs to be able to implement this.

