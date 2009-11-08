=======
xml2rst
=======

.. contents::

----------------
What is xml2rst?
----------------

`xml2rst` is a tool to generate reStructuredText_ syntax back from
`Docutils XML`_ input. This way one can create XML files using
`Docutils XML`_ from some other format (such as ODF_) and then
transform them to reStructuredText_.

It is currently implemented as an XSLT_ stylesheet.

------------
Availability
------------

`xml2rst` is available through the `Docutils Subversion repository`_
as part of the Docutils sandbox at
http://svn.berlios.de/viewcvs/docutils/trunk/sandbox/xml2rst

Moreover you can fetch it directly from the current maintainer at
http://www.merten-home.de/FreeSoftware/xml2rst/

------------
Installation
------------

`xml2rst` needs no special installation. Just use it with your
favorite XSLT processor such as Xalan_. Check Synopsis_ for
instructions how to do this.

---------------------
Copyright and license
---------------------

Copyright (C) 2005, 2006, 2009 by Stefan Merten and David Priest

License is GPL_ v2 or later.

-------------
Documentation
-------------

Synopsis
========

``Xalan`` `docutils.xml` ``xml2rst.xsl``

``testXSLT`` ``-xsl`` ``xml2rst.xsl`` ``-in`` `docutils.xml`

Options
=======

The following options are supported. They are XSLT parameters for the
whole script and must be given to the XSLT processor by the respective
option (Xalan: ``-p``, testXSLT: ``-param``).

-param ``adornment`` `adornment_configuration`
  Configures title markup to use so different styles can be requested
  easily.

  The value of the parameter must be a string made up of a sequence of
  character pairs. The first character of a pair is "o" (overline) or
  "u" (underline) and the second character is the character to use for
  the markup.

  The first and the second character pair is used for document title
  and subtitle, the following pairs are used for section titles where
  the third pair is used for the top level section title.

  Defaults to ``o=o-u=u-u~u:u.u``\ `````.

-param ``fold`` `folding_length`
  Configures whether long text lines in paragraphs should be folded
  and to which length. This option is for input not coming from reST
  which may have no internal line feeds in plain text strings.

  If folding is enabled text strings which are not in a linefeed
  preserving context are first white-space normalized and then broken
  according to the folding rules. Folding rules put out the first word
  and continue to do so with the following words unless the next word
  would cross the folding boundary. Words are delimited by
  white-space.

  Defaults to ``0``, i.e. no folding.

Unsupported features
====================

It is generally not possible to create an exact reproduction of an
original reStructuredText source from an intermediate XML file. The
reason is that Docutils transports pretty much but not all information
of the original source into the XML. Also the sequence of things is
changed sometimes.

However, the coverage of Docutils features of ``xml2rst`` is pretty
good. A few minor features are not supported:

* Fully minimized style for literal blocks

* Substitution references for ``replace::`` substitutions

* Counting roman numbers in enumerated lists

* Special table types like ``list-table::`` and ``csv-table::``

Example
=======

For a roundtrip try::

  rst2xml your_file.rst | xalan -xsl xml2rst.xsl | diff - your_file.rst

-----------
Development
-----------

ToDos
=====

The ``xml2rst.xsl`` contains a couple of comments marked with ``TODO``
which contain things which should be done.

.. ############################################################################

.. _reStructuredText: http://docutils.sourceforge.net/rst.html

.. _Docutils XML: http://docutils.sourceforge.net/docs/ref/doctree.html

.. _XSLT: http://www.w3.org/TR/1999/REC-xslt-19991116

.. _Docutils Subversion repository: http://docutils.sourceforge.net/docs/dev/repository.html

.. _Xalan: http://xalan.apache.org/

.. _GPL: http://www.gnu.org/copyleft/gpl.html

.. _ODF: http://www.oasis-open.org/committees/tc_home.php?wg_abbrev=office
