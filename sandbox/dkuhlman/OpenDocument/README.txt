=========
OdtWriter
=========


What is odtwriter?
==================

odtwriter is a back-end, writer for Docutils.  odtwriter produces
a .odt file that obeys the standards for ODF (Open Document
Format).  These files are usable in oowriter which is part of
OpenOffice.


Where to Find It
================

odtwriter is available through SVN (Subversion).  It is under
sandbox/dkuhlman/OpenDocument.  See:

- http://docutils.sourceforge.net/docs/dev/repository.html

- http://svn.berlios.de/viewcvs/docutils/trunk/


Documentation
=============

Documentation is in docs/odtwriter.txt/html.


Additional Information
======================

For more information on Docutils, see: http://docutils.sourceforge.net/


History
=======

2007/02/20 -- Version 1.0c
--------------------------

Renamed directive "syntaxhighlight" to "sourcecode".  Also changed
to a single argument, which can be "on" or "off" or <lexer-name>.


2007/01/15 -- Version 1.0c
--------------------------

Fix for footers -- Error traceback occurred when there is no
footer, i.e. no .. footer:: directive and none of 
--generator, --date, --time, --source-link, --source-url=URL.

Another fix for titles, headers, and footers -- The title of the
document was being shoved to the bottom of the document.

Enhancement to table of contents -- Now there are separate list
styles for list inside the table of contents and lists outside the
table of contents.  See styles rststyle-bulletlist,
rststyle-tocbulletlist, rststyle-enumlist, and
rststyle-tocenumlist.

2007/01/08 -- Version 1.0c
--------------------------

Added support for ..header:: and ..footer:: directives.  Added
styles rststyle-header and rststyle-footer.  The generator,
date/time, and generated-by decorations are now combined with the
content from the ..footer:: directive.

Made fix to literal_blocks so that interior spaces in a line are
preserved.


2007/01/05 -- Version 1.0c
--------------------------

Added support for the "table" directive.  Insert title into output
with new rststyle-table-title style.

Added limited support for "container" directive.  Limitations: (1)
Only the first class in the list of classes (arguments) is used
and (2) that class/style must be a paragraph style and not (for
example) a character style.


2006/12/31 -- Version 1.0b
--------------------------

Fixed imports to match latest version of Pygments.


2006/12/29 -- Version 1.0b
--------------------------

Added a separate lexer for LaTeX syntax highlighting.


2006/12/27 -- Version 1.0b
--------------------------

Added a Docutils directive which enables user (1) to turn syntax
highlighting in literal code blocks on and off and (2) to specify
which lexer (language) to use during syntax highlighing.

Updated the documentation to describe the syntax highlighting
directive.


2006/12/22 -- Version 1.0b
--------------------------

Implemented visit_line, depart_line, visit_line_block,
depart_line_block.

Implemented visit_subtitle and visit_subtitle as references to
visit_title and depart_title.


2006/12/19 -- Version 1.0b
--------------------------

Added syntax highlighting for literal code blocks.  Syntax
highlighting is applied only if Pygments is installed and the
--add-syntax-highlighting command line flag is used.  Pygments can
be found here:  http://pygments.pocoo.org/.  See the odtwriter
documentation for information about the styles used for syntax
highlighting.


2006/12/18 -- Version 1.0b
--------------------------

Fixed zipfile so that members of .odt have UNIX access permissions
and are stored deflated.


2006/12/07
----------

Fix for ElementTree getparent() and fixed zip DEFLATE.
- ElementTree does not implement getparent().  Created wrapper
  class to support this.
- Use of ZipInfo instances prevented compression.  Remove it.

Removed references to IPShell


2006/10/18
----------

Added support for images and figures.




