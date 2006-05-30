======================
Usage of Prest
======================
:Author: Mark Nodine
:Contact: mnodine@alum.mit.edu
:Revision: $Revision: 762 $
:Date: $Date: 2006-01-27 11:47:47 -0600 (Fri, 27 Jan 2006) $
:Copyright: This document has been placed in the public domain.

.. perl::
   # Make path safe for -T
   my $perl_dir = $1 if $^X =~ m|^(.*)/|;
   $ENV{PATH} = "$perl_dir:/bin";

.. contents::

This document gives the description and usage for the prest parser.  It
is compiled by running ``prest -h`` using a ``system::`` directive.

-------------

.. system:: perl -I .\./.\./blib/lib .\./.\./blib/script/prest -h 2>&1 | cat


..
   Local Variables:
   mode: indented-text
   indent-tabs-mode: nil
   sentence-end-double-space: t
   fill-column: 70
   End:
