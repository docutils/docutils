==========================
Restructure docutils' docs
==========================

Mentally separate / distinguish docutils from reStructuredText.

This is refactor that needs to be backed with redirects if it were to go
live. See `301 redirects`_ below.

Test
----

.. code-block:: bash

    $ ./tools/buildhtml.py . && python -m SimpleHTTPServer

todo
----

Reorganize files inside of reST folder
""""""""""""""""""""""""""""""""""""""

After a preliminary look ``/docs/reST/{howto,user}/`` may be better if
consolidated into ``/docs/reST/usage/``.

/docs/reST "Hub" page
"""""""""""""""""""""

/docs/reST would be served best by an index page which lays out the
available docs for the user. 

This can be organized in a visually friendly fashion and incorporate
touches of CSS for positioning.

Update internal links
"""""""""""""""""""""

Update internal links for all changed files.

301 redirects
"""""""""""""

For this to work successfully on the SourceForge project pages, without
breaking the old conventions, the old URL's must be made into ``301 Moved
Permanently`` redirects for the following::

    dev/rst
    > moved to reST/dev
    dev/rst/alternatives.html
    dev/rst/problems.html
    howto/rst-directives.html
    > moved to reST/howto/rst-directives.html
    howto/rst-roles.html
    > moved to reST/howto/rst-roles.html
    reST
    reST/dev
    > moved from dev/rst
    reST/howto
    reST/howto/rst-directives.html
    > moved from howto/rst-directives.html
    reST/howto/rst-roles.html
    > moved from howto/rst-roles.html
    reST/ref
    reST/ref/definitions.html
    > moved from ref/rst/definitions.html
    reST/ref/directives.html
    > moved from ref/rst/directives.html
    reST/ref/introduction.html
    > moved from ref/rst/introduction.html
    reST/ref/restructuredtext.html
    > moved from ref/rst/restructuredtext.html
    reST/ref/roles.html
    > moved from ref/rst/roles.html
    reST/user
    reST/user/cheatsheet.html
    > moved from user/rst/cheatsheet.html
    reST/user/demo.html
    > moved from user/rst/demo.html
    reST/user/images
    > moved from user/rst/images
    reST/user/quickref.html
    > moved from user/rst/quickref.html
    reST/user/quickstart.html
    > moved from user/rst/quickstart.html
    ref/rst
    ref/rst/definitions.html
    > moved to reST/ref/definitions.html
    ref/rst/directives.html
    > moved to reST/ref/directives.html
    ref/rst/introduction.html
    > moved to reST/ref/introduction.html
    ref/rst/restructuredtext.html
    > moved to reST/ref/restructuredtext.html
    ref/rst/roles.html
    > moved to reST/ref/roles.html
    user/rst
    user/rst/cheatsheet.html
    > moved to reST/user/cheatsheet.html
    user/rst/demo.html
    > moved to reST/user/demo.html
    user/rst/images
    > moved to reST/user/images

According to `SourceForge Project Web Services`_ there is ``mod_rewrite``
support.

Example of ``.htaccess`` rule::

    Options +FollowSymlinks -MultiViews
    RewriteEngine On
    RewriteBase /
    RewriteRule ^user/rst/demo.html  /reST/user/demo.html [R=301,NC,L]

.. _Google understands the refresh Meta tag:
   https://support.google.com/webmasters/answer/79812?hl=en
.. _SourceForge Project Web Services: 
   http://sourceforge.net/p/forge/documentation/Project%20Web%20Services/
