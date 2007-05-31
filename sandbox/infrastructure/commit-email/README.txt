=========================================
 File name patch for ``commit-email.pl``
=========================================

:Author: Lea Wiemann
:Contact: LeWiemann@gmail.com
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.


`<commit-email.patch>`_ is a patch against ``commit-email.pl`` of
Subversion 1.1.3.  Note that the patch cannot be applied to newer
versions of ``commit-email.pl``.

It adds file names to the subject lines, which would otherwise only
list directories.  The Docutils Subversion repository currently uses
this patched version of ``commit-email.pl`` to generate the check-in
messages sent to the Docutils-checkins mailing list.

The patch has been placed in the public domain.
