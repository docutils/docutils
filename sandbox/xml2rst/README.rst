=======
xml2rst
=======

.. contents::

What is xml2rst?
================

`xml2rst` is a tool to generate reStructuredText_ syntax back from
`Docutils XML`_ input. This way one can create XML files using
`Docutils XML`_ from some other format (such as ODF_) and then
transform them to reStructuredText_.

.. _flavor:

It is currently implemented as an XSLT_ stylesheet coming in three
flavors:

1. ``xml2rst.xsl``

   This version uses EXSLT_ and should be functionally equivalent to
   the old version. Because using EXSLT_ this version needs EXSLT_
   capable XSLT_ processors such as xsltproc_ [#deb-xsltproc]_.

   This version is currently maintained.

   .. [#deb-xsltproc] Under Debian based operating systems try
      ``apt-get install xsltproc``

2. ``xml2rst.py``

   This script uses ``xml2rst.xsl`` but through the XSLT_ engine
   available through the lxml_ package [#deb-lxml]_.

   .. [#deb-lxml] Under Debian based operating systems try ``apt-get
      install python-lxml``

3. ``xml2rst-noexslt.xsl``

   This version can be processed with every XSLT_ processor like
   Xalan_ [#deb-xalan]_.

   This version is no longer maintained, though.

   .. [#deb-xalan] Under Debian based operating systems try ``apt-get
      install xalan``

Availability
============

`xml2rst` is available through the `Docutils Subversion repository`_
as part of the Docutils sandbox at
http://svn.berlios.de/viewcvs/docutils/trunk/sandbox/xml2rst

Moreover you can fetch it directly from the current maintainer at
http://www.merten-home.de/FreeSoftware/xml2rst/

Installation
============

Depending on the flavor_ you choose you need to install certain
packages to run `xml2rst`. If using an XSLT_ processor try ``perldoc
xml2rst.xsl`` for instructions how to run it. If using the script try
``perldoc xml2rst.py``.

Copyright and license
=====================

Copyright (C) 2005, 2006 by Stefan Merten and David Priest
Copyright (C) 2009, 2010 by Stefan Merten

License is GPL_ v2 or later.

Example
=======

For a roundtrip try::

  rst2xml your_file.rst your_file.xml ; xml2rst.py your_file.xml | diff - your_file.rst

Development
===========

ToDos
-----

``xml2rst.xsl`` and ``xml2rst.py`` contain a couple of comments marked
with ``TODO`` which contain things which should be done.

.. ############################################################################

.. _reStructuredText: http://docutils.sourceforge.net/rst.html

.. _Docutils XML: http://docutils.sourceforge.net/docs/ref/doctree.html

.. _XSLT: http://www.w3.org/TR/1999/REC-xslt-19991116

.. _Docutils Subversion repository: http://docutils.sourceforge.net/docs/dev/repository.html

.. _xalan: http://xalan.apache.org/

.. _GPL: http://www.gnu.org/copyleft/gpl.html

.. _ODF: http://www.oasis-open.org/committees/tc_home.php?wg_abbrev=office

.. _EXSLT: http://www.exslt.org/

.. _xsltproc: http://xmlsoft.org/XSLT/xsltproc2.html

.. _lxml: http://codespeak.net/lxml/
