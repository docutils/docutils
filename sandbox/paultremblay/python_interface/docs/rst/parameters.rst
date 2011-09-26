####################
XSL-FO Documentation
####################

^^^^^^^^^^
Parameters
^^^^^^^^^^

.. contents:: Table of Contents

strict
======

**Possible Values**: ``True``, ``False``, ``''``

**Default:**

If set to True, stylesheet processing quits when an error is
found.

page-layout
===========

**Possible Values**: ``simple``, ``first``, ``odd-even``,
``first-odd-even``

**Default:**

This parameter determines the page layout for the document. A
value of ``simple`` will create a document with the same page
layout for all pages. A value of ``first`` creates a document
with a dfferent page layout for the first page and for the rest
of the pages. A value of ``odd-even`` creates a different layout
for for odd and even pages. A value of ``first-odd-even`` creates
a different layout for the first page, for odd pages, and for
even pages.

Because restructured text only allows one footer and header, the
footer and header will be the same for bth odd and even pages.
However, if the ``first`` or ``first-odd-even`` values is chosen,
you can suppress the first footer and header (see below).

Using a value other than ``simple`` allows for different margins
for different page sequences, depending on the value.

custom-spacing-header-footer
============================

**Possible Values**: ``boolean``

**Default:**

Tells the stylesheets to use attributes of body and region-before
that will determine the spacing for the header. Normally, the
stylesheets sets the area to .75in, if a relevant header or
footer is found. When ``'custom-spacing-header-footer'`` is set
to true, the stylesheets won't try to generate any spacing, but
will require these attributes to be set in the relevant attribute
sets.

pagination for front matter
===========================

**Possible Values**: ``with-front``, ``with-toc``, ``with-body``

**Defaults:** See below

The function is the same for the following parameters:

* title-pagination

* bibliographic-pagination

* dedication-pagination

* abstract-pagination

* toc-pagination

Each determines what region to place the textual matter. There
are three regions, the front matter, the toc matter, and the body
matter. The front matter has no footers and headers. The toc
matter starts a new page run, in which the numbers start with 1
(or any other value), and can take any formatting. The body
matter again starts a new run of pages with its own page
numbering and formatting of these numbers.

In practice, the abstract and title page often occurr before the
other front matter material, and they appear on pages with no
footers and headers. The dedication and Table of Contents appear
next, with the first numbering of the document, the numbers being
formatted as lower-case Roman numberals. The bibliographic
information could appear in either the front matter or toc
matter. In order to achieve this standard layout, the defaults
choose a ``with-front`` for the ``title-pagination``,
``abstract-pagination.``, and ``bibliographic-pagination; and a
``with-toc`` for the ``toc-pagination`` and
``dedication-pagination``.

In order to change these defaults, choose a different value. For
example, to place the dedication in the front matter, set
``dedication-pagination`` to ``with-front``. For a simple
document, in which there is only one set of page runs, simply set
each of these parameters to ``with-body``.

front-order
===========

**Possible Values**: ``title``, ``bibliographic``,
``dedication``, ``abstract``, ``toc``

**Default:** title,bibliographic,dedication,abstract,toc

The param ``front-order`` is a string of each region, separated
by a comma, that determines the order of the title, the
bibliographic information, the dedication, the abstract, and the
Table of Contents. The default puts them in order that docutils
puts them in when the document is converted to XML. In order to
change this, change the order in the string. For example, to
place the abstract before the dedication, use
``'title,bibliographic,dedication,abstract,toc'`` as a value.

If you have a region in your parameter value that does not
actually exist in your document, no error will occurr. For
example, if you set your value to
``title,bibliographic,dedication,abstract,toc``, but have no
``title`` in your document, the XSL stylesheet will still place
the abstract before the dedication without raising any error.

However, if you lack a region in your value that exists in the
document, the stylesheets will recognize this as an error,
notifiy you, and quit. For eaxmple, if your value is
``,bibliographic,dedication,abstract,toc``, and your document
contains a title, the processing will quit.

bibliographic-format
====================

**Possible Values**: ``list``, ``normal``, ``''``

**Default:** list

Determines the formatting of the bibliographic info. When set to
``'list'`` (the default), the bibliograhic fields will be
formatted as a list. When set to ``'normal'`` or ``''``, the each
bibliographic field will be formatted as a block.

Custom Table Columns
====================

**Possible Values**: ``numbers separated by commas``

**Defaults:** ''

The function is the same for the following parameters:

* table-cols

* table-borderless-cols

* table-long-cols

* table1-cols

* table2-cols

* ...

* table30-cols

Each parameter sets the columns for the table. ``'table-cols'``
sets the columns for the default table;
``'table-borderless-cols'`` sets the columns for the borderless
table, and ``'table-long-cols'`` sets the columns for the long
table. There are also 30 custom tables, and the parameter for the
columns is ``'table1-cols'``, ``'table2-cols'`` ...
``'table30-cols'``.

Use these parameters to override the defualts created by
rst2xml.py

Use a value of numbers separated by commas. For example, a value
of ``'10,20,10'`` sets the first column to 10, the second to 20,
and the third to 10. That means the first and third columns will
have the same width, and the second will be twice as large as
those.

long-rows-first-page
====================

**Possible Values**: ``numbers separated by commas``

**Default:**

Use this property to set the number of rows you want on the first
page for a table with a class of ``'long'``. Only use if you
desire a different caption from that which appears on the first
page.

FO by itself cannot create different table headings or footings
from on subsequent pages. The stylesheets get around this
limitation by creating two tables, one which takes the first
heading (or footing), and one which takes the second. The user
must tell the stylesheets when to start the new table; the
stylesheets have no way of calcuating this on their own.

Use numbers separated by commas for this parameter, where the
first number inidcatetes the first long table, the second the
second table, and so on. For example, a value of ``'8,10'`` tells
the stylesheet to break the first *long* table at 8 rows, and
the second at 10 rows.

Leave this parameter empty, or set it to 0 in order to have the
same caption on all pages.

Bibliographic Field Names
=========================

**Possible Values**: Any Text

**Defaults:** See below

The function is the same for the following parameters:

* author-text (default: Author: )

* authors-text (default: Authors: )

* organization-text (default: Organization: )

* contact-text (default: Contact: )

* status-text (default: Status: )

* copyright-text (default: Copyright: )

* address-text (default: Address: )

* version-text (default: Version: )

* revision-text (default: Revison: )

* date-text (default: Date: )

Each parameter sets the text in the list for that particular
bibliographic item. For example if you wanted to change the
default for ``contact`` from 'contact' to email, you would simply
set this value to 'email'.

Custom bibliographic field names
================================

**Possible Values**: Any Text

**Defaults:** ''

The function is the same for the following parameters:

* custom-bib-info1-name

* custom-bib-info2-name

* custom-bib-info3-name

* custom-bib-info4-name

* custom-bib-info5-name

* custom-bib-info6-name

* custom-bib-info7-name

* custom-bib-info8-name

* custom-bib-info9-name

* custom-bib-info10-name

Each parameter sets the value of the corresponding text for cutom
bibliographic fields.

Admonition Title Names
======================

**Possible Values**: Any Text

**Defaults:** See below

The function is the same for the following parameters:

* attention-title (default: Attention!)

* caution-title (default: Caution!)

* danger-title (default: !Danger!)

* error-title (default: Error)

* hint-title (default: Hint)

* important-title (default: Important)

* note-title (default: Note)

* tip-title (default: Tip)

* warning-title (default: Warning!)

Each parameter sets the text for the title for that particular
Admonition. For example if you wanted to change the default for
``attention-title`` from 'Important' to 'Pay Attention!', you
would simply set this value to 'Pay Attnetion!'.

transition-text
===============

**Possible Values**: Any Text

**Defaults:** \*\*\*

The text to use for a transtion element. Use any text (including
an empty string) to change that value.

Formatting of Section Numbering
===============================

**Possible Values**: Valid Number Formatting String

**Defaults:** See below

The function is the same for the following parameters:

* number-section1 (default: 1)

* number-section2 (default: .1)

* number-section3 (default: .1)

* number-section4 (default: .1)

* number-section5 (default: .1)

* number-section6 (default: .1)

* number-section7 (default: .1)

* number-section8 (default: .1)

* number-section9 (default: .1)

Each parameter sets the formatting (not the actual number) for
that particular level. The stylesheets allow for a great deal of
flexibility here. For example, in order to set a level 3 number
format to '(II)3.b', you would set ``number-section1`` to '(I)',
``number-section2`` to '.1' (the default, in this case, meaning
you woud not need to make a change), and ``number-section3`` to
'.a'.

inherit-section-num
===================

**Possible Values**: ``True``, ``False``

**Default:** True

If set to 'True', each section inherits the section numbering
from the sections above it. For example, section '1.1.2' will
appear as '1.1.2'. If set to 'False', the section number will
appear as '2'.

bullet-text
===========

**Possible Values**: Any Text

**Default:** •

Use to set the value for the bullets in a bullet list. If this
string is left blank, then the stylesheets will use the value in
the XML.

bullet-text-level2
==================

**Possible Values**: Any Text

**Default:** °

Use to set the value for the bullets in a nested bullet list. If
this string is left blank, then the stylesheets will use the
value in the XML.

option-list-format
==================

**Possible Values**: ``list``, ``definition``

**Default:** list

Use to determine the formatting of an options list. If ``list``
is choosen, then the options list is formatted as a traditional
list, with the options to the left and the description to the
right. If ``definition`` is choosen, the options list is
formatted as a defintion list, with the options above the
description, which is indented. Lists with long options are
probably better formatted using ``definition.``

options-separator
=================

**Possible Values**: Any Text

**Default:** ,

Use to set the value for the text that separates the options in
an option list. For example, if your RST file has ``-f -file`` as
the options, and you choose ';' as the ``options-separator``,
the output becomes ``-f; -file``.

number-verse
============

**Possible Values:** any positive integer, or ``''``

**Default:** 5

When set, this parameter numbers a line block ("verse") every
``value`` lines. The value of ``'5'`` numbers every 5th line. If
``number-verse`` is left empty, the line block will not be
numbered.

Text Before Attributions
========================

**Possible Values**: Any Text

**Defaults:** —

The function is the same for the following parameters:

* text-before-block-quote-attribution

* text-before-epigraph-attribution

* text-before-pull-quote-attribution

Each parameter determines the text before the attribution. When
the parameter is left empty, no text will appear before an
attribution.

table-title-placement
=====================

**Possible Values**: ``top``, ``bottom``

**Default:** bottom

Where to place the table title, or caption.

footnote-placement
==================

**Possible Values**: ``footnote``, ``endnote``

**Default:** footnote

This parameter determines whether footnotes will function as
footnotes, or endnotes. When ``footnote`` is choosen, footnotes
appear at the bottom of the page. When ``endnote`` is choosen,
the *numbered* footnotes appear as endnotes, in the same position
where they are in the RST document. If ``endnote`` is choosen,
symbolic footnotes still appear as footnotes, thus giving a user
the ability to use both footnotes and endnotes.

footnote-style
==============

**Possible Values**: ``list``, ``traditional``

**Default:** list

This parameter determines the style of the footnote or endnote
text. When ``'list'``, is choosen, the text is formatted as a
list, with the number as the item. When ``'traditional'`` is
choosen, the footnote appears in the more traditional manner, as
a paragraph with the first line indented.

space-between-footnotes
=======================

**Possible Values**: Any Measure

**Default:** 5pt

Use to set the space between footnotes. (I have not determined
how to set this property in the normal way, which is why this
property appears as a parameter, rather than in an attribute set,
like the other similar properties.)

internal-link-type
==================

**Possible Values**: ``link``, ``page``, ``page-link``

**Default:** link

When set to ``'page'``, the page number of the target appears.
When set to ``'link'``, the text of the link appears, and
clicking on that link takes you to the target. When set to
``'page-link'``, the page of the target appears, and clicking on
that page number takes you to the target.

test
====

**Possible Values**: ``True``, ``False``, ``''``

**Default:**

For testing purposes only.

