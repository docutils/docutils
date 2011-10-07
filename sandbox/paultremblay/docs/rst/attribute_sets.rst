####################
XSL-FO Documentation
####################

^^^^^^^^^^^^^^
Attribute Sets
^^^^^^^^^^^^^^

.. contents:: Table of Contents

Root Attribute Sets
===================

Attribute sets root elements. Use these attribute sets to format
the defaults in a document, such as font, font-size, or
line-height.

page-sequence
------------------

:fo: fo:page-sequence

:defaults:

     format: 1

     initial-page-number: 1

Formats the properties for the complete run of pages, in this
case, the body. You could use this attribute set to set the font for the
entire document, for example.

paper-size
-----------------------------

:fo: None


:defaults:

     page-width: 8.5in

     page-height: 11in

Sets up the the paper size.

page-margins
-----------------------------

:fo: None

:defaults:

	margin-left: 1.0in
	margin-right: 1.0in
	margin-top: 1.0in
	margin-bottom: 1.0in

Sets up the the margins.

region-body
-----------------------------

:fo: region-body

:defaults:

        margin-top: .75in
        margin-bottom: .75in

Sets the defaults for the margins for the body region (as opposed to the
page). These defaults are only used if headers or footers are found.

region-before
-----------------------------

:fo: region-before

:defaults:

    extent: .75in

Sets the extent for the region-before. This attribute set will only be used if
a header is found.

region-after
-----------------------------

:fo: region-after

:defaults:

    extent: .75in

Sets the extent for the region-after. This attribute set will only be used if
a footer is found.



body-flow
---------

:fo: fo:flow


:defaults:

Formats the properties of the body in the body sequence of pages,
which means everything except headers and footers.

footnote-separator-flow
-----------------------

:fo: fo:flow

:defaults:

Formats the flow of the footnote.

footnote-separator-block
------------------------

:fo: fo:block

:defaults:

Formats the block (with the leader) that separates the footnotes
from the rest of the page.

Header and Footer Attribute Sets
================================

paragraph-header-block
----------------------

:fo: fo:block

:defaults:

     font-size: 12pt

     text-align: center

Formats the properties for the paragraphs in the header. Use to
set the space between the footer and the body text, using
``'space-before = x'`` and setting
``'space-before.conditionality'`` to ``'retain'``.

paragraph-footer-block
----------------------

:fo: fo:block

:defaults:

     font-size: 12pt

     text-align: center

Formats the properties for the footer. Use to set the space
between the footer and the body text, using ``'space-before =
x'`` and setting ``'space-before.conditionality'`` to
``'retain'``.

Bibliograhic Fields Attribute Sets
==================================

Attribute sets for the bibliograhic fields. These attributes
control the formatting of bibliographic fields.

bibliographic-fields-list-block
-------------------------------

:fo: list-block

:defaults:

     start-indent: 0mm

     provisional-distance-between-starts: 30mm

     space-before: 12pt

     space-after: 12pt

Formats the bibliographic fields as a list. Since this element
contains all the other list elements, it can be used to set
values such as the font, background color, line-height, etc, for
the entire list, as well as the space after and before.

"The provisional-distance-between-starts property of the
list-block specifies the distance bewteen the start of the label
(the bullet, for example) and the actual start of the list
content" (Pawson, 100). In this case, that means the distance
between the label (such as "Version", and the labels' value (such
as "1.2").

bibliographic-fields-list-item
------------------------------

:fo: fo:list-item


:defaults:

     space-before: 12pt

For each item (author, authors, organization, contact, address,
version, date, copyright, custom field) in the bibliograhic
fields. Use the 'space-after' attribute to control the spacing
between each item.

bibliographic-fields-first-list-item
------------------------------------

:fo: fo:list-item

:inherits: bibliographic-fields-list-item

:defaults:

     space-before: 0pt

Same as above, but sets the space before to 0pt.

bibliographic-fields-list-item-label
------------------------------------

:fo: fo:list-item-label


:defaults:

     end-indent: label-end()

The default attribute end-indent = "label-end()" ensures that the
label aligns properly.

bibliographic-fields-item-body
------------------------------

:fo: fo:list-item-body

:defaults:

     start-indent: body-start()

The default of start-indent = "body-start()" ensures the correct
alignment of the labels.

bibliographic-fields-item-label-block
-------------------------------------

:fo: fo:block

:defaults:

     font-weight: bold

Formats the block that wraps the the name of the field (such as
Author, Version, etc).

bibliographic-fields-block
--------------------------

:fo: fo:block


:defaults:

Formats the blocks (docutilis paragraphs) of the value of the
field. Use the 'space-after' attribute to control the spacing
between a multi-paragraph description.

bibliographic-first-fields-block
--------------------------------

:fo: fo:block


:inherits: bibliographic-fields-block

:defaults:

Same as above, but for the first such paragraph.

address-value-block
-------------------

:fo: fo:block


:inherits: bibliographic-fields-block

:defaults:

     white-space: pre

Formats the blocks (docutilis paragraphs) of the address field,
which has to preserve the white space, according to the docutils
specs. Since this inherits from the bibliographic-fields-bloc, it
doesn't make sense to change attributes here directly.

Footnote
========

Attribute sets for footnotes, endnotes, and the endnotes title.

footnote
--------

:fo: fo:footnote

:defaults:

     font-weight: normal

     font-style: normal

Formats the footnote. By default, it sets properties to neutral,
so that it does not inherit any unwanted properties, such as from
a definition term.

default-footnote-label-inline
-----------------------------

:fo: fo:inline

:defaults:

     baseline-shift: super

     font-size: 8pt

Sets of the defaults for the label (1, \*, etc), of each label.


footnote-list-block
-------------------

:fo: fo:list-block

:defaults:

     provisional-label-separation: 0pt

     provisional-distance-between-starts: 18pt

Formats the list that contains the footnote. The
'provisional-distance-between-starts' controls how far away the
footnote label is from the text.

footnote-item-label
-------------------

:fo: fo:list-item-label

:defaults:

     end-indent: label-end()

Formats the item-label when the footnote or endnote is formatted
as a list.

footnote-label-block
--------------------

:fo: fo:block

:defaults:

Formats the block in item-label when the footnote or endnote is
formatted as a list. By default, the label has no superscript (as
opposed to when formatting a "traditional" footnote.

footnote-item-body
------------------

:fo: fo:list-item-body

:defaults:

     start-indent: body-start()

Formats the item-body when the footnote or endnote is formatted
as a list.

footnote-body
-------------

:fo: fo:footnote-body

:defaults:

Formats the body of the footnote. Space-after and space-before
seem to have no affect, at least with fop.

footnote-paragraph-block
------------------------

:fo: fo:block

:defaults:

     space-before: 5pt

Formats the paragraphs in the body of a footnote or endnote. Use
the 'space-before' to set the space between each paragraphs, for
footnotes or endnotes with multiple paragraphs. 


Endnote
========

For attributes when the endnotes.xsl stylesheet is imported.

endnote-block
-------------

:fo: fo:block

:defaults:

     space-before: 5pt

The block that wraps each individual endnote ('footnote' in
docutils). Use to control the spacing between each endnote.

endnote-first-block
-------------------

:fo: fo:block

:inherits: endnote-block

:defaults:

     space-before: 0pt

The block that wraps each the first endnote ('footnote' in
docutils). It does not make sense to change attributes on this
set directly.

endnotes-title-block
--------------------

:fo: fo:block

:defaults:

     space-after: 18pt

     font-weight: bold

     font-size: 18pt

     text-align: center

Formats the title for the endnotes, when one is present. The rst will have a
rubric with the classes as "endnotes. The XML will look like <rubric
@classes="endotes">

TOC Matter Attribute Sets
=========================

Attribute sets for the TOC.

toc-title-block
---------------

:fo: fo:block

:defaults:

     text-align: center

     font-weight: bold

     font-size: 14pt

Formats the block for the title for the TOC.

toc-entry-defaults-block
------------------------

:fo: None

:defaults:

     space-after: 3pt

     text-align-last: justify

Sets up the defaults for the TOC entries.

toc-level1-block
----------------

:fo: fo:block


:inherits: toc-entry-defaults-block

:defaults:

Formats the block for the level 1 table of contents entry. If a
number exists, it is formatted according to the parameter
'number-section1'.

toc-level2-block
----------------

:fo: fo:block

:inherits: toc-entry-defaults-block

:defaults:

     start-indent: 10mm

Formats the block for the level 2 table of contents entry. If a
number exists, it is formatted according to the parameter
'number-section2'.

toc-level3-block
----------------

:fo: fo:block

:inherits: toc-entry-defaults-block

:defaults:

     start-indent: 20mm

Formats the block for the level 3 table of contents entry. If a
number exists, it is formatted according to the parameter
'number-section3'.

toc-level4-block
----------------

:fo: fo:block

:inherits: toc-entry-defaults-block

:defaults:

     start-indent: 30mm

Formats the block for the level 4 table of contents entry. If a
number exists, it is formatted according to the parameter
'number-section4'.

toc-level5-block
----------------

:fo:


:inherits: toc-entry-defaults-block

:defaults:

     start-indent: 40mm

Formats the block for the level 5 table of contents entry. If a
number exists, it is formatted according to the parameter
'number-section5'.
