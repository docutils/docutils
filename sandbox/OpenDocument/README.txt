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

2008/10/23 -- Version 1.3c
--------------------------

Fixes for line blocks.  These are now indented correctly, I
believe, even when inside a block quote.  There are new styles for
line blocks (rststyle-lineblock1, ...).  See the doc.

Fixes to footnotes.  Auto-numbered and auto-symbol footnotes seem
to work correctly now.  There is a constant (footnote_chars)
containing the symbols (*, **, ***, ++, etc).  

Added support for citations.

Fixed sections -- Now start at correct level.

Added styles rststyle-title and rststyle-subtitle.

Various source code clean-ups.

Added style rststyle-footnote and rststyle-citation.


2008/07/08 -- Version 1.3b
--------------------------

Added support for sub-script and super-script (:sub: and :sup:).

Added support for classifier on definition list terms.


2008/07/03 -- Version 1.3a
--------------------------

Added support for the meta directive.  See docs.

Added support for additional visit_/depart_ methods.

Regularized some of the methods that generated fields: revision,
version, date, address, contact, copyright, organization, etc.

Added more control for control of paper size, thanks to Michael
Schutte:

1. __init__.py attempts to run paperconf.  If paperconf succeeds
   and if the styles.odt file does not contain paper size, the size
   is inserted into the styles.xml in the generated document.

2. Added rst2odt_prepstyles.py script -- Drop page size
   specifications from styles.xml in STYLE_FILE.odt.

Added tools/rst2odt_prepstyles.py to the distribution.



2008/04/18 -- Version 1.2b
--------------------------

Increased maximum header levels.  Added new header styles
rststyle-heading6 and rststyle-heading7 to styles.odt.

Fixed bug related to lists inside of definition of a definition
list.  Fixed handling of indentation levels.  Thanks to Stefan
Merten for help with this.


2008/03/20 -- Version 1.2a, again
---------------------------------

Fix for footnotes -- Removed extra space between footnote reference
and preceding text.

Added support for the Docutils raw directive.  Raw XML content is
now parsed to produce an ElementTree subtree which is inserted
into the ODF content tree.  See the doc (section "The raw
directive") for a few notes on using the raw directive with
odtwriter.


2008/01/06 -- Version 1.2a
--------------------------

Added Stefan Merten's implementation of custom style names.

A few miscellaneous fixes, e.g. some related to the Pygments
source code highlighting.

Added an empty/do-nothing implementation for the raw directive.


2008/01/06 -- Version 1.1a, again
---------------------------------

Fixes to figures and images -- The caption on a figure is
displayed below the image in a space that is the same width as the
image.  Also did some clean-up to the code that determines the
size of images and the code that generates them.


2007/12/20 -- Version 1.1a, again
---------------------------------

Removed extra numbers in table of contents and section titles when
the sectnum directive is used.


2007/12/20 -- Version 1.1a
--------------------------

Another image fix -- If PIL is installed and if the height and
width of an image are not specified in an image directive and if
the scale is specified in an image, then odtwriter attempts to use
PIL/Image to determine the size (height, width) of the image before
scaling.

Document title: (1) The document title and top level section
headings were both being given style rststyle-heading1.  Now,
document title gets rststyle-heading1 and top level sections get
rststyle-heading2.  Basically, section heading levels are bumped up
by 1. (2) The command line flag --title is now implemented.  It
overrides the document title in the source document if present.


2007/12/19 -- Version 1.1a
--------------------------

Minor fixes for images.  But, we still cannot position an image
correctly *within* a paragraph.


2007/12/07 -- Version 1.1a
--------------------------

Fixes to sourcecode highlighting.

Fixes to images:

- Eliminated storing the same image multiple times in the .odt file.

- Fix to images defined in image substitution definitions.


2007/10/05 -- Version 1.1a
--------------------------

Adapted odtwriter to the stronger XML namespace handling in lxml 2.0.


2007/10/01 -- Version 1.0e, again
---------------------------------

A significant improvement to the content generated for literal
blocks thanks to a patch from Johan Holmberg.


2007/03/14 -- Version 1.0e
--------------------------

Fix so that rst.Directive class is not used for older versions of
Docutils.  They do not implement that class.

Fix so that if ElementTree is in the standard Python library,
odtwriter will also look for it there.


2007/03/14 -- Version 1.0d
--------------------------

Added support for highlights block/directive.

Fixed styles in highlights, epigraphs, and blockquotes.  Bullet and
enum lists now have their own styles in each of these block types.


2007/03/12 -- Version 1.0d
--------------------------

Added support for epigraph and the associated attribution.  Added
and fixed styles for epigraph, attribution, and block-quote.

Added support for line-block.  Added style for line blocks.

Added support for substitution (replace directive).  Actually, the
substitution is done before odtwriter starts walking the tree. 
But, I added the visit/depart methods so as to eliminate the error
messages.

Fixed visit_Text so that it can add *multiple* text nodes that are
all children of the same node.


2007/03/09 -- Version 1.0d
--------------------------

Fix for non-utf-8 character sets, e.g. Russian.


2007/03/08 -- Version 1.0d
--------------------------

Added support for footnotes.


2007/02/21 -- Version 1.0c
--------------------------

Fixed headers and footers.  Added support for references/URLs.


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




