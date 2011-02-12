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
* left-indent [have to create this short form]
* right-indent [have to create this short form]
* border (for the boders around the block) [have to create this short form]
* line-spacing (for double space, for exampe)
* alignment [have to creat his short form]
* keep-with-next (to keep the block with the next block, to avoid widows
  and orphans)
* keep-with-previous (to keep the block with the previous block, to avoid widows
  and orphans)
* keep-together (to prevent a block form splitting across a page)
* break-after (to create page breaks) [have to create short form]
* break-before (to create page breaks) [have to create short form]

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
