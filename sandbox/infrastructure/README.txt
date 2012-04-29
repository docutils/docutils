==========================
 Docutils_ Infrastructure
==========================

:Author: Lea Wiemann
:Contact: grubert@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.


The `infrastructure <.>`_ sandbox directory stores any scripts that
are needed for the development of the Docutils project.

TODOs:

* subversion is now on sourceforge
  - change release and update

* sourceforge does not have cron, but berlios installation
  did not work since long time.

* clean up.
  
Overview:

Everything below this needs rework.

:`<docutils-update.local>`_: The script to update the `web site`_
    from a developer machine. *The* current way to go.

:`<release.sh>`_: The script to make releases_ of Docutils.

:`<uploaddocutils.sh>`_: Upload files to http://docutils.sf.net/tmp/
    using ``scp``, inserting the current date in the file name.

:`<update-htmlfiles>`_: Used to initialise a docutils-update upload directory.
    Generating html-files from txt-files first time.

:`<htmlfiles.lst>`_: The list of files for ``update-htmlfiles``.

and are these used by anyone.

:`<dbackport.sh>`_: Back-port changes from the trunk to the
    maintenance branch.

:`<fsfsbackup.sh>`_: Backup (mirror) an FSFS Subversion repository via
    SSH.  Used to backup the `Docutils Subversion repository`_.

.. _Docutils: http://docutils.sourceforge.net/
.. _Docutils check-in mailing list:
   http://docutils.sf.net/docs/user/mailing-lists.html#docutils-checkins
.. _web site: http://docutils.sourceforge.net/docs/dev/website.html
.. _Docutils Subversion repository:
   http://docutils.sourceforge.net/docs/dev/repository.html
.. _releases: http://docutils.sourceforge.net/docs/dev/release.html
