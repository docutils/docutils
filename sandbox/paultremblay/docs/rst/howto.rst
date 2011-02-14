^^^^^^^^^^^^^^^^^^^^^^^^
HOWTO: Docutils2fo 0.6
^^^^^^^^^^^^^^^^^^^^^^^^

..  $Id$ 

.. contents::

================
Convert to XML
================

Use the rst2xml.py script, and pass the the ``'--strip-comments'`` and 
``'--trim-footnote-reference-space'`` options::

 rst2xml.py --strip-comments --trim-footnote-reference-space my_file.rst > my_file.xml

================
Convert to FO 
================

Use the docutils_to_fo.py script::

 docutils_to_fo.py  my_file.xml > my_file.fo

Use the ``'--help'`` option to get options.

Alternatively, you can convert from RST to FO in one step::

 rst2xml.py --strip-comments --trim-footnote-reference-space my_file.rst | docutils_to_fo.py > my_file.fo

================
Convert to PDF 
================

Use the fop processor, an open source Java application at http://xmlgraphics.apache.org/fop/.
In order to run in from a unix system::

 fop my_file.fo my_file.pdf

To convert from RST to PDF with one command::

 rst2xml.py --strip-comments --trim-footnote-reference-space my_file.rst\
 | docutils_to_fo.py | fop -fo - my_file.pdf

===================
Format a Document
===================

1. Create a config file that with a section and instructions (as explained
   below)::

    [FO]

    paragraph.space-before = 12pt


2. Enable the ``'docutils_to_fo'`` script to read the config file. Either pass 
   the ``'--config'`` option::

     docutils_to_fo.py  --config my_config_file my_file.xml > my_file.fo

  or put this file where the script automatically reads it, either in 
  ``./docutils.conf``, or ``~/home/.docutils``. 

Unless otherwise noted, the rest of this document refers to creating the
config file.

==================
Create a Command
==================

Commands consist of instructions without a period in them. The following
sets ``'strict'`` = ``'True'``, making the processing quit when an error is 
encountered::

 strict = True 

 

================================
Create a Formatting Instruction
================================

Formatting instructions consist of 3 parts: the region to be formatted,
followed by a period; the formatting property, followed by an equal sign; and
the value of the property. The following command sets the space before plain
paragrahs to 12pt::

 paragraph.space-before = 12pt

.. tip::

 You can use the config file to pass commonly-used options of the 
 rst2xml.py command. Put these commands in the *general* section::

   [general]

   strip_comments = true
   trim-footnote-reference-space = true

   [FO]
   paragraph.space-before = 12pt


==============================
General Formatting for Blocks
==============================

Most of the regions in a docutils document consists of blocks. This is true of
the title, subtitle, section titles, paragrahs, block quotes, etc. All of thes
blocks can take the following properties:

* space-before
* space-after
* first-line-indent
* font (the same as font-family)
* font-size
* font-style
* color (for the color of the text)
* backgroud-color
* left-indent 
* right-indent 
* border (for the boders around the block) 
* line-spacing (for double space, for exampe)
* alignment 
* keep-with-next (to keep the block from ending a page)
* keep-with-previous (to keep the block from starting a new page)
* keep-on-same-page (to prevent a block form splitting across a page)
* page-break-before (to create page breaks) 
* page-break-after (to create page breaks) 

These properties are the most commonly used and have an easy-to-remember,
short name. In addition, one can use any of the valid FO properties, which can
be found, among other places, at http://www.w3schools.com/xslfo/obj_block.asp.
For example, ``'paragraph.border-before-color = red'`` specifies the color of the 
border on the before-edge for plain paragraphs.

============
Page Layout
============

Paper Size
-----------

::

 paper-size.height = 11in
 paper-size.width = 8.5in

Margins
---------
::

 page.top-margin = .75in
 page.bottom-margin = .75in
 page.right-margin = .75in
 page.left-margin = .75in

Different Margins for Odd and Even Pages (if desired)
------------------------------------------------------
::

 odd-page.top-margin = 1in
 odd-page.bottom-margin = 1in
 odd-page.right-margin = 1in
 odd-page.left-margin = 2in
 
 even-page.top-margin = 1in
 even-page.bottom-margin = 1in
 even-page.right-margin = 2in
 even-page.left-margin = 1in

Different Margins for the First Page (if desired)
---------------------------------------------------
::

 first-page.top-margin = 3in
 first-page.bottom-margin = 1in
 first-page.right-margin = .8in
 first-page.left-margin = 2in


====================
Headers and Footers
====================

Create a header
---------------

In the *document* (not the config file), use the following::

 .. header:: 

    A Christmas Carol 

Create a footer
---------------

In the *document* (not the config file), use the following::

 .. footer:: 

    Charles Dickens 

Create a page number in a header or footer
--------------------------------------------

In the *document* (not the config file), use the following::

 .. role:: page-num
    
 .. footer:: 

    Charles Dickens.
    
    :page-num:`1`

Page numbers will be generated automatically.

Create space for a header or footer
------------------------------------

The defaults should create enough space for headers or footers. If you want to
change the defaults, use::

 header.height = 1in
 footer.height = 1in


Create space between header and body text
------------------------------------------

::

 header.space-before = .2in
    
Create space between footer and body text
------------------------------------------

::

 footer.space-before = -.1in

Suppress first page header or footer
--------------------------------------

::

 footer.suppress-first-page = True
 header.suppress-first-page = True

Create custome headers and footers for odd, even, etc.
--------------------------------------------------------

There is no way to create different headers and footers for different parts of
the document, since Restructured Text only allows for one type of header and
footer.

Format header and footer text
------------------------------

If your header or footer contains just one paragraphs, use the header or
footer region plus any block property::

 header.color = blue
 header.background-color = black

If you have more than one paragraph, you can either set all the properties the
same using as in the example above, or set each individual paragraph. You can
format up to 3 different paragraphs:: 

 # create a blue backgroud color for all paragraphs
 header.background-color = blue
 # create different formatting for each paragraph
 header-first-paragraph.font-style = bold
 header-second-paragraph.font-style = italic
 header-third-paragraph.font-style = italic-bold

 footer-first-paragraph.font-style = bold
 footer-second-paragraph.font-style = italic
 footer-third-paragraph.font-style = italic-bold

===========
Paragraphs
===========

Space before or after paragraphs
---------------------------------

::

 paragraph.space-before = 12pt
 paragraph.space-after = 12pt

Indent first line
-------------------

::

 paragraph.first-line-indent = 12pt

Font Family
-------------

::

 paragraph.font = monospace

Font Size
----------

::

 paragraph.font-size = 12pt

Font Style
-----------

Set ``'font-style'`` to ``'bold'``, ``'italic'``, ``'bold-italic'``,
or ``'normal'``::

 paragraph.font-style = bold-italic

Color (font color)
-------------------

::

 paragraph.color = red

Background color
------------------

::

 paragraph.backgroud-color = red

Left and right indent
----------------------

::

 paragraph.left-indent = 12pt 
 paragraph.right-indent = 12pt 

Borders
---------

::

 paragraph.border = solid black 1px

Use ``'border-top'``, ``'border-bottom'``, ``'border-left'``, and
``'border-right'`` to set properties on specific borders::

 # creates a bottom border
 paragraph.border-bottom = solid black 1px

Line spacing
--------------

::

 # double spaces paragraphs
 paragraph.line-spacing  = 2

Alignment
-----------

Use ``'alignment'`` to align text. Valid values are ``'left'``,
``'right'``, ``'center'``, and ``'justify'``.

::

 # center aligns text
 paragraph.alignment = center

Keep with previous text
-------------------------

::

 # paragrah won't start a new page
 keep-with-previous = true

Keep  with next text
----------------------

::

 # paragrah won't end the page
 keep-with-next = true

Keep from breaking across pages
---------------------------------
::

 keep-on-same-page = true


Page break before or after
----------------------------

::

 paragraph.page-break-before = yes
 paragraph.page-break-after = yes


No page break before or after
-------------------------------

::

 paragraph.page-break-after = no
 paragraph.page-break-before = no


