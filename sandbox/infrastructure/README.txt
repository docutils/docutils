==========================
 Docutils_ Infrastructure
==========================

:Author: Felix Wiemann
:Contact: Felix.Wiemann@ososo.de
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.


The `infrastructure <.>`_ sandbox directory stores any scripts that
are needed for the development of the Docutils project.

Overview:

:`<commit-email>`_: A patch to commit-email.pl used for the `Docutils
    check-in mailing list`_.

:`<dbackport.sh>`_: Back-port changes from the trunk to the
    maintenance branch.

:`<docutils-update>`_: The script that updates the `web site`_.

:`<fsfsbackup.sh>`_: Backup (mirror) an FSFS Subversion repository via
    SSH.  Used to backup the `Docutils Subversion repository`_.

:`<release.sh>`_: The script used to make releases_ of Docutils.

:`<uploaddocutils.sh>`_: Upload files to http://docutils.sf.net/tmp/
    using ``scp``, inserting the current date in the file name.

.. _Docutils: http://docutils.sourceforge.net/
.. _Docutils check-in mailing list:
   http://docutils.sf.net/docs/user/mailing-lists.html#docutils-checkins
.. _web site: http://docutils.sourceforge.net/docs/dev/website.html
.. _Docutils Subversion repository:
   http://docutils.sourceforge.net/docs/dev/repository.html
.. _releases: http://docutils.sourceforge.net/docs/dev/release.html
