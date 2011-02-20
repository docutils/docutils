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


================
Non Body Matter
================

Non Body matter refers to the document title and subtitle; the
bibliographic fields; the dedication; the abstract; and the table of
contents. 

Each such matter can be put in the front part, the toc (table of
contents) part, or the body part. 

The front part of a document occurrs first and has not headers or
footers. The toc part occurrs next, generally starts with page 1
(thought this can be changed), and page numberas are  formamtted with
Roman numberals (though this can be changed.)

The body part is part of any document. It generally starts with page 1
(though this can be changed) and page numbers are formatted with
Arabic numbers (though this can be changed).

Placing the title/subtitle
---------------------------

With the front part (default)
+++++++++++++++++++++++++++++++

::

 title-subtitle.placement = with-front

With the toc part
++++++++++++++++++

::

 title-subtitle.placement = with-toc

With the body part
+++++++++++++++++++

::

 title-subtitle.placement = with-body

Placing the bibliograph fields
-------------------------------

With the front part 
++++++++++++++++++++

::

 bibliographic-fields.placement = with-front

With the toc part (default)
++++++++++++++++++++++++++++

::

 bibliographic-fields.placement = with-toc

With the body part
+++++++++++++++++++

::

 bibliographic-fields.placement = with-body

Placing the dedication
-----------------------

With the front part 
++++++++++++++++++++

::

 dedication.placement = with-front

With the toc part (default)
++++++++++++++++++++++++++++

::

 dedication.placement = with-toc

With the body part
+++++++++++++++++++

::

 dedication.placement = with-body

Placing the abstract
-----------------------

With the front part (default)
++++++++++++++++++++++++++++++

::

 abstract.placement = with-front

With the toc part 
++++++++++++++++++

::

 abstract.placement = with-toc

With the body part
+++++++++++++++++++

::

 abstract.placement = with-body

Placing the toc
----------------

With the front part 
++++++++++++++++++++

::

 toc.placement = with-front

With the toc part 
++++++++++++++++++

::

 toc.placement = with-toc

With the body part
+++++++++++++++++++

::

 toc.placement = with-body

Changing the order of non-body matter
---------------------------------------

By default, docutils to fo places the non-body matter in the order it
occurrs in the docutils XML document (which may be different than the
RST document). This order is title/subtitle, bibliographic,
dedication, and table of contents. To change this order, use the
``'front.order'`` property. Its value is a list of the non-body order
areas in the order you wish it to occurr, speparated by commas::

 # changes the order of the abstract and the bibliographic fields
 front.order = title, abstract, dedication, toc, bibliographic

Changing the starting page number
----------------------------------

In order to change the starting page number of a toc section or body
section, use the ``'toc-section.start-page'`` or
``'body-section.start-page'`` properties::

 # toc now start on page 3
 toc-section.start-page = 3
 # body starts on page 15
 body-section.start-page = 15

Changing the format of the page number
----------------------------------------

In order to change the formatting of the page numbers, use the
``'toc-section.page-format'`` and ``'body-section.page-format'``::

 # upper case Roman numberals
 toc-section.page-format = I
 # upper case letters
 body-section.page-format = A

Formatting the title/subtitle
------------------------------

Use the property ``'title-subtitle'`` to format both the title and
subtitle. Use the property ``'title'``  to format just the title. Use the
property ``'subtitle'`` to format just the subtitle. All of these
properites are block properties, and can take any of the
properites of a block (see paragraph)::

 # center the title an subtitle
 title-subtitle.alignment = center
 # format the title
 title.font-size = 24pt
 # format the subtitle
 subtitle.font-size = 18pt

To create space before a title/subtitle, and to put the title/subtitle
on their own page::

 title-subtitle.page-break-before= true
 title-subtitle.space-before=3in

===================================
Formatting the bibliograhic fields
===================================

Formatting as a list
---------------------

By defaullt, bibliographic fields are formatted as a list. Use the
``'bibliograhic-fields'`` property to format the space after and before, the
left and right indent, and any other property you want to set on the list,
such as font for font-size::

 bibliographic-fields.space-after = 20pt
 bibliographic-fields.space-before = 20pt
 bibliographic-fields.left-indent = 20pt
 bibliographic-fields.right-indent = 20pt
 bibliographic-fields.background-color = grey

Putting the bibliographic fields on its own page
++++++++++++++++++++++++++++++++++++++++++++++++++

::

 bibliographic-fields.page-break-before = true
 # or, depending on your layout
 bibliographic-fields.page-break-after = true


To format space between items
++++++++++++++++++++++++++++++

Use the ``'space-between-items'`` property.

::


  bibliographic-fields.space-between-items = 30pt

To format space between field and text
+++++++++++++++++++++++++++++++++++++++

The ``'space-from-fields'`` works exactly as the ``'space-from-bullet''``: both
properties control the spacing from the list label and the text.

::

  bibliographic-fields.space-from-field = 3in

Formatting the field
++++++++++++++++++++++

Use the ``'bibliographic-field'`` property, and apply any block properties.
Note the slight, one letter difference between ``'bibliographic-fields``, which
formats the entire list, and ``'bibliographic-field'`` which formats just the
field::

 # change default font to normal
 bibliographic-field.font-style = normal

Changing the default text of the fields
+++++++++++++++++++++++++++++++++++++++++

By default, the docutils to FO converter gives expected names to each of the
bibliographic fields. If you wish to change the name, use the
``'bibliographic-fields.<fieldname-text>`` property::

  # change all the fields from the default to lower case
  bibliographic-fields.author-text = author
  bibliographic-fields.authors-text = authors
  bibliographic-fields.organization-text = organization
  # change 'contact' to 'email'
  bibliographic-fields.contact-text = email
  bibliographic-fields.status-text = status
  bibliographic-fields.copyright-text = copyright
  bibliographic-fields.address-text = address
  bibliographic-fields.revision-text = revision
  bibliographic-fields.date-text = date

Formatting the text of the field
+++++++++++++++++++++++++++++++++++

Use the ``'bibliographic-fields-text'`` identifier::

 bibliographic-fields-text.font-weight=bold

Formatting the text for individual paragraphs
+++++++++++++++++++++++++++++++++++++++++++++

For fields that have more than one paragraph, use the ``'bibliographic-fields-paragraph'``
identifier. This identifier can take any block property::

 bibliographic-fields-paragraph.space-before = 12pt

Note that using the ``'space-before'`` property has the same effect as
controlling the space between each paragraph, without putting unwated space
before the first paragraph. 

Formatting as blocks of text
-----------------------------

Use ``'the bibliographic-fields.format'`` to change the value of the default
format::

 bibliographic-fields.format = normal

Formatting the author field, etc.
----------------------------------

Each bibliographic field is a block, and its identifier is the name of the
field . 

:address: formats the address field
:author: formats the author field
:authors: formats the authors field
:contact: formats the contact field
:copyright: formats the copyright field
:date: formats the date field
:organization: formats the organization field
:revision: formats the revision field
:status: formats the status field
:version: formats the version field

::

 address.space-before=24pt
 author.font-style = bold
 authors.alignment = center
 contact.font-style = italic
 copyright.color = red
 date.background-color = blue
 organization.font-size = 24pt
 revision.alignment = center
 status.alignment = right
 version.font-style = bold-italic
        

Formatting custom bibliographic fields
----------------------------------------

Use the ``'bibliographic-fields-custom1'``,
``'bibliographic-fields-custom2'``, etc, to format each custom field, where
``'-custom1'`` refers to the first occurrence of a custom field,
''`-custom2'`` refers to the second such occurrence, and so on:: 

 bibliographic-field-custom1.color = green
 bibliographic-field-custom2.color = red

Changing the default text of the fields
+++++++++++++++++++++++++++++++++++++++++

As when the bibliiographic fields are formatted as a list, the text of the
fields can likewise be changed in the same manner when formatting the
bibliographic fields as blocks. It often makes sense to makes these fields
empty, especially when creating title page::

  bibliographic-fields.author-text = by
  bibliographic-fields.date-text = 

Including text for custom fields
+++++++++++++++++++++++++++++++++

Use the ``'bibliographic-field-custom#.text'`` property::

 bibliographic-field-custom1.text = Country
 bibliographic-field-custom2.text = Uses of software: 

Making a Title Page
--------------------

Here is the actual RST document::

 ======
 Title
 ======
 
 Subtitle
 =========
 
 
 :by: by
 :Author: Paul Tremblay
 :Address: Paul Tremblay 
           100 Market St. 
           Boston, MA, 01800
 
 :Country: USA
 :Organization: Open Source Software
 :useline: Uses
 :Uses: text processing
 
        documentation
 :Date: $Date$
 :Copyright: This document is in the public domain
 
 Text after.

Here is the configuration file::

 [general]
 trim-footnote-reference-space = true
 [FO]
 bibliographic-fields.format = normal
 
 # the by line by itself
 bibliographic-field-custom1.space-after = 12pt
 bibliographic-field-custom1.alignment = center

 # the line with 'usline' 
 bibliographic-field-custom3.space-after = 0pt
 bibliographic-field-custom3.font-style = bold

 # the Uses field
 bibliographic-field-custom4.space-before = 0pt

 # center align author
 author.alignment = center
 author.font-size = 16pt
 # put a border after the title
 author.border-bottom = 2px solid
 author.padding = 6pt

 # put a borer after copyright
 copyright.border-bottom = 2px solid
 copyright.padding = 6pt
 
 
 
 # set the default text to nothing
 bibliographic-fields.author-text =
 bibliographic-fields.authors-text = 
 bibliographic-fields.organization-text = 
 bibliographic-fields.copyright-text = 
 bibliographic-fields.address-text = 
 bibliographic-fields.date-text = 

 # commands
 strict = True


Formatting the dedication
--------------------------

Creating a dedication
+++++++++++++++++++++++

::

 :Dedication: I dedicate this thesis to my good friend John, who
   stood by me for many years while I struggled ...

Putting dedication on its own page
++++++++++++++++++++++++++++++++++++++++++++++++++

::

 dedication.page-break-before = true
 # or, depending on your layout
 dedication.page-break-after = true

Formatting the title
+++++++++++++++++++++++

Use the ``'dedication-title'`` identifier, which can take any block property::

 dedication-title.alignment = center
 dedication-title.font-size = 24pt

Formatting the paragraphs
++++++++++++++++++++++++++

Use the ``'dedication-paragraph'`` identifier, which can take any block property::

 dedication-paragraph.font-size = 10pt
 dedication-paragraph.alignment = right
 dedication-paragraph.font-style = italic


Formatting the abstract
--------------------------

Creating a abstract
+++++++++++++++++++++++

::


 :Abstract: Just an example of bibliograhic fields.
  This continues on.

Putting abstract on its own page
++++++++++++++++++++++++++++++++++++++++++++++++++

::

 abstract.page-break-before = true
 # or, depending on your layout
 abstract.page-break-after = true

Formatting the title
+++++++++++++++++++++++

Use the ``'abstract-title'`` identifier, which can take any block property::

 abstract-title.alignment = center
 abstract-title.font-size = 24pt

Formatting the paragraphs
++++++++++++++++++++++++++

Use the ``'abstract-paragraph'`` identifier, which can take any block property::

 abstract-paragraph.font-size = 10pt
 abstract-paragraph.alignment = right
 abstract-paragraph.font-style = italic


Formatting the toc
--------------------------

Creating a toc
+++++++++++++++++++++++

::

 .. contents:: Table of Contents

Putting toc on its own page
++++++++++++++++++++++++++++++++++++++++++++++++++

::

 toc.page-break-before = true
 # or, depending on your layout
 toc.page-break-after = true

Formatting the title
+++++++++++++++++++++++

Use the ``'toc-title'`` identifier, which can take any block property::

 toc-title.alignment = center
 toc-title.font-size = 24pt

Setting the defaults on each entry
++++++++++++++++++++++++++++++++++++

Use the ``'toc-default'`` to set properties for all of the toc entries
at once::


 # sets space between entries to 12pt
 toc-default.space-after = 12pt


Formatting the entries
++++++++++++++++++++++++++

Use the identifierst ``'toc-entry1'``, ``'toc-entry2'``, etc.
which can take any block property::

 # increase indents by 10mm
 toc-entry1.left-indent = 10mm
 toc-entry2.left-indent = 20mm
 toc-entry3.left-indent = 30mm
 toc-entry4.left-indent = 40mm
 toc-entry5.left-indent = 50mm


Format the toc numbers
+++++++++++++++++++++++

The format of the numbers for toc entry takes the same format as the
section numbers. See section numbers.

=========
Sections
=========

Creating sections
------------------

Sections are identified through their titles, which are marked up with
adornment: "underlines" below the title text, or underlines and matching
"overlines" above the title.


Here are some examples::

 ===============
 Heading1 Title
 ===============
 
 ---------------
 Heading2 Title
 ---------------
 
 Heading3 Title
 =============
 
 Heading4 Title
 -------------
 
 Heading5 Title
 `````````````
 
 Heading6 Title
 '''''''''''''
 
 Heading7 Title
 .............
 
Any combination of valid adornments can be used. The rst2xml.py utility
recognizes the first such example as the main section, the next such example
as the sub section, and so on.

See the http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#sections.


Formatting titles
-------------------

Use the identifiers ``'heading1'``, ``'heading2'``, ``'heading3'``, etc, to
format the titles for each section. Docutils to fo allows sections to go 7
levels deep. Headings are blocks and can take any property of a block (see
paragrahs). 

::

 heading1.font-size = 30pt
 heading1.font-style = bold
 heading2.font-size = 24pt
 # etc.
 heading7.font-size = normal
 heading7.font-style = bold

Creating section numbers
-------------------------

At the start of the document, put::

 .. sectnum::

Formatting section numbers
---------------------------

Use the ``'heading#'`` identifier plus the ``'number-format'`` to format the
section numbers. The value for formatting can take a combination of
punctuation and numbers, letters, or Roman numberals

::

 # (I.), (II.), (III.), etc
 heading1.number-format = (I.)

 # i.), ii.), etc 
 heading2.number-format = i.)

 # .1., .2., .3., etc
 heading3.number-format = .1.

 # a, b, c, etc
 heading4.number-format = a

 # A, B, C., etc
 heading5.number-format = A

.. Don't inherit section numbers
.. ------------------------------
.. 
.. By default, the numbers of each section inherit the numbers of the parent
.. section. to disable this formatting::
.. 
..  headings.inherit-sections-number = False

============
Transitions
============

Creating transitions
---------------------

To create a transition, in the *document* (not the config file), use
horizontal line of 4 or more repeated punctuation characters.

::


 Para.

 ----------

 Para.

Changing the text of the transtion.
-------------------------------------

The default transition text is a single paragraph of three asteristics. To
change the default, use the ``'transition.text'`` property::

 # change to three hypens
 transition.text = ---
 # change to nothing
 transition.text = 

Formatting the transition paragraph
------------------------------------

Use the ``'transition'`` identifier to format the paragraph of the transition
text. This identifier can take any block property.

::

 transition.space-before = 24pt
 transition.space-after = 24pt
 # change the default alignment from center
 transition.alignment = left


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

Formatting for first paragrahs
--------------------------------

Docutils to FO allows first paragraphs to be formatted differently
from all others. First paragraphs refers to the first paragraphs after
a section of document, or the first paragaph after a list, code block,
admonition block, or table. British style dictates that such
paragraphs do not have their first line indented. 

::

 #indents all but first paragraph 12pt
 paragraph.first-line-indent = 12pt
 first-paragraph.first-line-indent = 0pt

============
Bullet List
============


Formatting the bullet list
----------------------------

Use the ``'bullet-list'`` property to format the space after and
before, the left and right indent, and any other property you want to
set on the list, such as font for font-size::

 bullet-list.space-after = 20pt
 bullet-list.space-before = 20pt
 bullet-list.left-indent = 20pt
 bullet-list.right-indent = 20pt
 bullet-list.background-color = grey


To format space between items
-------------------------------

Use the ``'space-between-items'`` property.

::


  bullet-list.space-between-items = 30pt

To format space between bullets and text
-------------------------------------------

Use the ``'space-from-bullet'`` identifier::


  bullet-list.space-from-bullet = 1in

Choosing the text for the bullet
-----------------------------------

Use the ``'bullet-list.text'`` property to change the default bullet::

 # change to hyphen
 bullet-list.text = -

If the ``'bullet-list.text'`` property is left empty, the converter
uses the text in the actual document::

 # use the text in the document 
 bullet-list.text = 


Formatting the  paragraphs
----------------------------

Use the ``'bullet-list-paragraph'`` identifier to format the text of
the bullet list. This identifier can take any block property::

 bullet-list-paragraph.space-before = 12pt
 bullet-list-paragraph.font-style = italic

Note that using the ``'space-before'`` property has the same effect as
controlling the space between each paragraph, without putting unwated space
before the first paragraph. 

Formatting nested bullet lists
-------------------------------

Use the ``'bullet-list-level2'`` to format nested lists::

 bullet-list-level2.space-before = 20pt
 bullet-list-level2.left-indent = 40pt
 bullet-list-level2.background-color = blue
 bullet-list-level2.space-from-bullet = 1.5in
 bullet-list-level2.text = †
 bullet-list-level2.space-between-items = 50pt


=================
Enumerated List
=================

Formatting the enumerated list
-------------------------------

Use the ``'enumerated-list'`` property to format the space after and
before, the left and right indent, and any other property you want to
set on the list, such as font for font-size::

 enumerated-list.space-after = 20pt
 enumerated-list.space-before = 20pt
 enumerated-list.left-indent = 20pt
 enumerated-list.right-indent = 20pt
 enumerated-list.background-color = grey


To format space between items
-------------------------------

Use the ``'space-between-items'`` property.

::


  enumerated-list.space-between-items = 30pt

To format space between enumerateds and text
---------------------------------------------

Use the ``'space-from-number'`` identifier::


  enumerated-list.space-from-number = 1in


Formatting the  paragraphs
---------------------------

Use the ``'enumerated-list-paragraph'`` identifier to format the text of
the enumerated list. This identifier can take any block property::

 enumerated-list-paragraph.space-before = 12pt
 enumerated-list-paragraph.font-style = italic

Note that using the ``'space-before'`` property has the same effect as
controlling the space between each paragraph, without putting unwated space
before the first paragraph. 

Formatting nested enumerated lists
-----------------------------------

Use the ``'enumerated-list-level2'`` to format nested lists::

 enumerated-list-level2.space-before = 20pt
 enumerated-list-level2.left-indent = 40pt
 enumerated-list-level2.background-color = blue
 enumerated-list-level2.space-from-number = 1.5in
 enumerated-list-level2.space-between-items = 50pt


=================
Definition List
=================

Formatting the definition list
-------------------------------

Use the ``'definition-list'`` property to format the space after and
before, the left and right indent, and any other property you want to
set on the list, such as font for font-size::

 definition-list.space-after = 20pt
 definition-list.space-before = 20pt
 definition-list.left-indent = 20pt
 definition-list.right-indent = 20pt
 definition-list.background-color = grey

Formatting space between items
-------------------------------

An item consists of both the term and definition.

::

 definition-list.space-between-items = 0pt

To format space below term
---------------------------

Use the ``'space-below-term'`` property.

::

  definition-list.space-below-term = 30pt

Formatting the term
---------------------

Use the ``'definition-term'`` identifier, which can take any block property::

 definition-term.right-indent = 10pt
 definition-term.color = red

Formatting the definition
--------------------------

Use the ``'definition-list-definition'`` identifier, which can take any block property::

 definition-list-definition.color = blue

Formatting the classifier
-------------------------

Use the ``'definition-list-classifier'`` identifier, which can take any inline property::

::

 definition-list-classifier.color = green

Formatting the  paragraphs
---------------------------

The ``'definition-list-definition'`` formats all the paragraphs in the
definiton. If you wish to change a property on the paragraphs instead, use the
``'definition-list-paragraph'`` identifier::

 # the space before dtermines the space between each paragraph
 definition-list-paragraph.space-before = 12pt

============
Field List
============


Formatting the field list
----------------------------

Use the ``'field-list'`` property to format the space after and
before, the left and right indent, and any other property you want to
set on the list, such as font for font-size::

 field-list.space-after = 20pt
 field-list.space-before = 20pt
 field-list.left-indent = 20pt
 field-list.right-indent = 20pt
 field-list.background-color = grey


To format space between items
-------------------------------

Use the ``'space-between-items'`` property.

::


  field-list.space-between-items = 30pt

To format space between field and text
-------------------------------------------

Use the ``'space-from-name'`` identifier::


  field-list.space-from-name = 1in

Formatting the field names
---------------------------

Use the ``'field-name'`` identifier, which can take any inline
properties::


  field-name.color = blue


Formatting the  paragraphs
----------------------------

Use the ``'field-list-paragraph'`` identifier to format the text of
the bullet list. This identifier can take any block property::

 field-list-paragraph.space-before = 12pt
 field-list-paragraph.font-style = italic

Note that using the ``'space-before'`` property has the same effect as
controlling the space between each paragraph, without putting unwated space
before the first paragraph. 


============
Option List
============

Choosing the layout
-------------------

::

 option-list.format = definition

Choosing the options separator
-------------------------------

By default, the docutils to FO convertor uses a comma to separate
options. To change the default, use the ``'options-list.separate'``
property::

 options-list.separator = :

Formatting the option list format
-----------------------------------

The option list can either be formatted as a list, with the options as
labels to the left of the description; or as a definition list, with
the options serving as the terms, and the descriptions in a paragraph
right below. For an option list with lenghty options, a definition
list may work better.

Use the ``'option-list.format'`` to determine the style, choosing
either ``'list'`` or ``'definition'``::

 # change default layout to a definitio list
 option-list.format = definition

Formatting the option list
----------------------------

Use the ``'option-list'`` property to format the space after and
before, the left and right indent, and any other property you want to
set on the list, such as font for font-size::

 option-list.space-after = 20pt
 option-list.space-before = 20pt
 option-list.left-indent = 20pt
 option-list.right-indent = 20pt
 option-list.background-color = grey


To format space between items
-------------------------------

Use the ``'space-between-items'`` property.

::


  option-list.space-between-items = 30pt
 

To format space between option and text
-------------------------------------------

Use the ``'space-from-option'`` identifier::


  option-list.space-from-option = 1in

Note: this option is only valid for option lists formatted as lists,
not for optons lists formatted as definition lists.

To format space below option and text
-------------------------------------------

Use the ``'space-below-option'`` identifier::


  option-list.space-below-option = 1in

Note: this option is only valid for option lists formatted as
definition lists, not for optons lists formatted as lists.


Formatting the options
---------------------------

Use the ``'options'`` identifier to format the option with the
arguments of the options. This identifier  can take any inline
properties::


  options.font-size = xx-small

Formatting the options without the argument
----------------------------------------------

Use the ``'option'`` identifier to format only the option without the
arguments of the options. This identifier  can take any inline
properties::


  option.color = green

.. option-group-block

Formatting the arguments of the options
----------------------------------------------

Use the ``'option-argument'`` identifier to format just the option of
the arugment. This identifier  can take any inline properties::


  option-argument.color = green


Formatting the description
----------------------------

Use the ``'option-list-body'`` identifier to format the text of
the option list::

 option-list-body.font-style = italic

..  option-list-description-block

Formatting the paragraphs
----------------------------

Use the ``'option-list-paragraph'`` identifier to format the text of
the bullet list. This identifier can take any block property::

 option-list-paragraph.space-before = 12pt
 option-list-paragraph.font-style = italic

Note that using the ``'space-before'`` property has the same effect as
controlling the space between each paragraph, without putting unwated space
before the first paragraph. 

