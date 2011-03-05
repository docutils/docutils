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

default-page-sequence
---------------------

:fo: fo:page-sequence

:docutils: document

:defaults:

Formats the properties for the all of the document.

front-page-sequence
-------------------

:fo: fo:page-sequence

:docutils: document

:inherits: default-page-sequence

:defaults:

     force-page-count: no-force

Formats the properties for the complete run of pages, in this
case, the front matter.

toc-page-sequence
-----------------

:fo: fo:page-sequence

:docutils: document

:inherits: default-page-sequence

:defaults:

     format: i

     initial-page-number: 1

     force-page-count: no-force

Formats the properties for the complete run of pages, in this
case, the toc and any pages associated with it.

body-page-sequence
------------------

:fo: fo:page-sequence

:docutils: document

:inherits: default-page-sequence

:defaults:

     format: 1

     initial-page-number: 1

Formats the properties for the complete run of pages, in this
case, the body.

default-flow
------------

:fo: fo:flow

:docutils: document

:defaults:

Sets up the default attributes for the front-flow, toc-flow, and
body-flow

front-flow
----------

:fo: fo:flow

:docutils: document

:inherits: default-flow

:defaults:

Formats the properties of the body in the front sequence of
pages. Since the front sequence has no headers and footers, that
means everything.

toc-flow
--------

:fo: fo:flow

:docutils: document

:inherits: default-flow

:defaults:

Formats the properties of the body in the toc sequence of pages,
which means everything except headers and footers.

body-flow
---------

:fo: fo:flow

:docutils: document

:inherits: default-flow

:defaults:

Formats the properties of the body in the body sequence of pages,
which means everything except headers and footers.

footnote-separator-flow
-----------------------

:fo: fo:flow

:docutils: footnote

:defaults:

Formats the flow of the footnote.

footnote-separator-block
------------------------

:fo: fo:block

:docutils: footnote

:defaults:

Formats the block (with the leader) that separates the footnotes
from the rest of the page.

Page Attribute Sets
===================

Attribute sets for page. These attributes control the formatting
of the actual pages: the paper size and margins.

paper-size-simple-page-master
-----------------------------

:fo: None

:docutils: /

:defaults:

     page-width: 8.5in

     page-height: 11in

Sets up the defaults for the paper size, used in other attribute
sets.

default-simple-page-master
--------------------------

:fo: None

:docutils: /

:defaults:

     margin-left: 1.0in

     margin-right: 1.0in

     margin-top: 1.0in

     margin-bottom: 1.0in

Sets up the defaults for the margins of the fo:body-region for
all the pages.

Simple Page Master Sets
-----------------------

:fo: fo:simple-page-master

:docutils: /

:inherits: paper-size, default-page-setup

The following attribute sets are identical:

- simple-page-master

- first-simple-page-master

- body-simpe-page-master

- odd-simple-page-master

- even-simple-page-master

- toc-simple-page-master

- toc-first-simple-page-master

- toc-body-simple-page-master

- toc-odd-simple-page-master

- toc-even-simple-page-master

- front-simple-page-master

- front-first-simple-page-master

- front-body-simple-page-master

- front-odd-simple-page-master

- front-even-simple-page-master

These attriute sets format the margins of the
fo:simple-page-master. By default, they inherit the
``'paper-size-simple-page-master'`` and
``'default-simple-page-master'`` attriute-sets, meaning each page
will have identical size and margins, a satisfactory setup for
many documents. However, the sizes and margins can be modified by
page type, if desired.

header-region-before
--------------------

:fo: fo:region-before

:docutils: decoration/header

:defaults:

     extent: .75in

The extent attribute specifies the header and footer height.

footer-region-after
-------------------

:fo: fo:region-after

:docutils: decoration/footer

:defaults:

     extent: .75in

The extent attribute specifies the header and footer height.

Bibliograhic Fields Attribute Sets
==================================

Attribute sets for the bibliograhic fields. These attributes
control the formatting of bibliographic fields.

bibliographic-fields-list-block
-------------------------------

:fo: list-block

:docutils: docinfo

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

:docutils:
         docinfo/author|docinfo/authors|docinfo/organization|etc.

:defaults:

     space-before: 12pt

For each item (author, authors, organization, contact, address,
version, date, copyright, custom field) in the bibliograhic
fields. Use the 'space-after' attribute to control the spacing
between each item.

bibliographic-fields-first-list-item
------------------------------------

:fo: fo:list-item

:docutils:
         docinfo/author|docinfo/authors|docinfo/organization|etc.

:inherits: bibliographic-fields-list-item

:defaults:

     space-before: 0pt

Same as above, but sets the space before to 0pt.

bibliographic-fields-list-item-label
------------------------------------

:fo: fo:list-item-label

:docutils:
         docinfo/author|docinfo/authors|docinfo/organization|etc.

:defaults:

     end-indent: label-end()

The default attribute end-indent = "label-end()" ensures that the
label aligns properly.

bibliographic-fields-item-body
------------------------------

:fo: fo:list-item-body

:docutils:
         docinfo/author|docinfo/authors|docinfo/organization|etc.

:defaults:

     start-indent: body-start()

The default of start-indent = "body-start()" ensures the correct
alignment of the labels.

bibliographic-fields-item-label-block
-------------------------------------

:fo: fo:block

:docutils:
         docinfo/author|docinfo/authors|docinfo/organization|etc.

:defaults:

     font-weight: bold

Formats the block that wraps the the name of the field (such as
Author, Version, etc).

bibliographic-fields-block
--------------------------

:fo: fo:block

:docutils:
         docinfo/author|docinfo/authors|docinfo/organization|etc.

:defaults:

Formats the blocks (docutilis paragraphs) of the value of the
field. Use the 'space-after' attribute to control the spacing
between a multi-paragraph description.

bibliographic-first-fields-block
--------------------------------

:fo: fo:block

:docutils:
         docinfo/author|docinfo/authors|docinfo/organization|etc.

:inherits: bibliographic-fields-block

:defaults:

Same as above, but for the first such paragraph.

address-value-block
-------------------

:fo: fo:block

:docutils: docinfo/address

:inherits: bibliographic-fields-block

:defaults:

     white-space: pre

Formats the blocks (docutilis paragraphs) of the address field,
which has to preserve the white space, according to the docutils
specs. Since this inherits from the bibliographic-fields-bloc, it
doesn't make sense to change attributes here directly.

author-block
------------

:fo: fo:block

:docutils: docinfo/author

:defaults:

     space-before: 12pt

Formats the author element, when bibliograhic info is not
formatted as a list.

authors-block
-------------

:fo: fo:block

:docutils: docinfo/authors

:defaults:

     space-before: 12pt

Formats the authors element, when bibliograhic info is not
formatted as a list.

date-block
----------

:fo: fo:block

:docutils: docinfo/date

:defaults:

     space-before: 12pt

Formats the date element, when bibliograhic info is not formatted
as a list.

organization-block
------------------

:fo: fo:block

:docutils: docinfo/organization

:defaults:

     space-before: 12pt

Formats the organization element, when bibliograhic info is not
formatted as a list.

contact-block
-------------

:fo: fo:block

:docutils: docinfo/contact

:defaults:

     space-before: 12pt

Formats the contact element, when bibliograhic info is not
formatted as a list.

status-block
------------

:fo: fo:block

:docutils: docinfo/status

:defaults:

     space-before: 12pt

Formats the status element, when bibliograhic info is not
formatted as a list.

copyright-block
---------------

:fo: fo:block

:docutils: docinfo/copyright

:defaults:

     space-before: 12pt

Formats the copyright element, when bibliograhic info is not
formatted as a list.

version-block
-------------

:fo: fo:block

:docutils: docinfo/version

:defaults:

     space-before: 12pt

Formats the version element, when bibliograhic info is not
formatted as a list.

revision-block
--------------

:fo: fo:block

:docutils: docinfo/revision

:defaults:

     space-before: 12pt

Formats the revision element, when bibliograhic info is not
formatted as a list.

address-block
-------------

:fo: fo:block

:docutils: docinfo/address

:defaults:

     white-space: pre

     space-before: 12pt

Formats the address element, when bibliograhic info is not
formatted as a list.

Custom bibliographic fields
---------------------------

:fo: fo:block

:docutils: docinfo/field

The following attribute sets are identical in nature:

* custom-bib-info1

* custom-bib-info2

* custom-bib-info3

* custom-bib-info4

* custom-bib-info5

* custom-bib-info6

* custom-bib-info7

* custom-bib-info8

* custom-bib-info9

* custom-bib-info10

These attribute-sets format the custom bibliographic fields.
``'custom-bib-info1'`` refers to the first occurrence of such a
field, ``'custom-bib-info2'`` to the second, and so fourth.

Front Matter Attribute Sets
===========================

Attribute sets for the dedication and abstract.

dedication-block
----------------

:fo: fo:block

:docutils: topic[@classes = "dedication"]

:defaults:

Formats the dedication text, including the title and subsequent
paragraphs, by wrapping them in a block.

abstract-block
--------------

:fo: fo:block

:docutils: topic[@classes = "abstract"]

:defaults:

Formats the abstract text, including the title and subsequent
paragraphs, by wrapping them in a block.

dedication-title-block
----------------------

:fo: fo:block

:docutils: topic[@classes = "dedication"]/title

:defaults:

     text-align: center

     font-weight: bold

     space-after: 12pt

Formats the title for the dedication.

abstract-title-block
--------------------

:fo: fo:block

:docutils: topic[@classes = "abstract"]/title

:defaults:

     text-align: center

     font-weight: bold

Formats the abstract title.

dedication-paragraph-block
--------------------------

:fo: fo:block

:docutils: topic[@classes = "dedication"]/paragraph

:defaults:

     font-style: italic

     space-after: 12pt

Formats the paragraphs of the dedication.

dedication-first-paragraph-block
--------------------------------

:fo: fo:block

:docutils: topic[@classes = "dedication"]/paragraph

:inherits: dedication-paragraph-block

:defaults:

     space-before: 0pt

Formats the first paragraph of the dedication.

abstract-paragraph-block
------------------------

:fo: fo:block

:docutils: topic[@classes = "abstract"]/paragraph

:defaults:

     space-before: 12pt

Formats the paragraphs of the abstract.

abstract-first-paragraph-block
------------------------------

:fo: fo:block

:docutils: topic[@classes = "abstract"]/paragraph

:inherits: abstract-paragraph-block

:defaults:

     space-before: 0pt

Formats the first paragraph of the abstract.

Header and Footer Attribute Sets
================================

Attribute sets for the headers and footers. Since headers and
footers often need very special formatting, the stylesheets allow
for the formatting of up to three paragraphs for each header and
footer. The first refers to the first that occurrs in the
document, the second to the second, and the third to the third.

header-block
------------

:fo: fo:block

:docutils: decoration/header

:defaults:

     font-size: 12pt

     text-align: center

     space-before.conditionality: retain

Formats the properties for the header. Use to set the space
between the header and the body text, using ``'space-before =
x'`` and setting ``'space-before.conditionality'`` to
``'retain'``.

first-header-block
------------------

:fo: fo:block

:docutils: container[@classes='first-header']

:defaults:

Formats the properties for the header for the first page.

odd-header-block
----------------

:fo: fo:block

:docutils: container[@classes='odd-header']

:defaults:

Formats the properties for the for the header of odd pages, when
using a layout of ``'first-odd-even'``, or ``'odd-even'``.

even-header-block
-----------------

:fo: fo:block

:docutils: container[@classes='even-header']

:defaults:

Formats the properties for the for the header of even pages, when
using a layout of ``'first-odd-even'``, or ``'odd-even'``.

body-header-block
-----------------

:fo: fo:block

:docutils: container[@classes='body-header']

:defaults:

Formats the properties for the for the header of the body, when
using a layout of ``'first'``.

footer-block
------------

:fo: fo:block

:docutils: decoration/footer

:defaults:

     font-size: 12pt

     text-align: center

     space-before.conditionality: retain

Formats the properties for the footer. Use to set the space
between the header and the body text, using ``'space-before =
x'`` and setting ``'space-before.conditionality'`` to
``'retain'``.

first-footer-block
------------------

:fo: fo:block

:docutils: container[@classes='first-footer']

:defaults:

Formats the properties for the footer for the first page.

odd-footer-block
----------------

:fo: fo:block

:docutils: container[@classes='odd-footer']

:defaults:

Formats the properties for the for the footer of odd pages, when
using a layout of ``'first-odd-even'``, or ``'odd-even'``.

even-footer-block
-----------------

:fo: fo:block

:docutils: container[@classes='even-footer']

:defaults:

Formats the properties for the for the footer of even pages, when
using a layout of ``'first-odd-even'``, or ``'odd-even'``.

body-footer-block
-----------------

:fo: fo:block

:docutils: container[@classes='body-footer']

:defaults:

Formats the properties for the for the footer of the body, when
using a layout of ``'first'``.

toc-first-header-block
----------------------

:fo: fo:block

:docutils: container[@classes='toc-first-header']

:defaults:

Formats the properties for the header for the first page of the
TOC.

toc-odd-header-block
--------------------

:fo: fo:block

:docutils: container[@classes='toc-odd-header']

:defaults:

Formats the properties for the for the header of odd pages of the
TOC, when using a layout of ``'first-odd-even'``, or
``'odd-even'``.

toc-even-header-block
---------------------

:fo: fo:block

:docutils: container[@classes='toc-even-header']

:defaults:

Formats the properties for the for the header of even pages of
the TOC, when using a layout of ``'first-odd-even'``, or
``'odd-even'``.

toc-body-header-block
---------------------

:fo: fo:block

:docutils: container[@classes='toc-body-header']

:defaults:

Formats the properties for the for the header of the body of the
TOC, when using a layout of ``'first'``.

toc-first-footer-block
----------------------

:fo: fo:block

:docutils: container[@classes='toc-first-footer']

:defaults:

Formats the properties for the footer for the first page of the
TOC.

toc-odd-footer-block
--------------------

:fo: fo:block

:docutils: container[@classes='toc-odd-footer']

:defaults:

Formats the properties for the for the footer of odd pages of the
TOC, when using a layout of ``'first-odd-even'``, or
``'odd-even'``.

toc-even-footer-block
---------------------

:fo: fo:block

:docutils: container[@classes='toc-even-footer']

:defaults:

Formats the properties for the for the footer of even pages of
the TOC, when using a layout of ``'first-odd-even'``, or
``'odd-even'``.

toc-body-footer-block
---------------------

:fo: fo:block

:docutils: container[@classes='toc-body-footer']

:defaults:

     font-size: 12pt

     text-align: center

     space-before.conditionality: retain

Formats the properties for the for the footer of the body of the
TOC, when using a layout of ``'first'``.

paragraph-header-block
----------------------

:fo: fo:block

:docutils: decoration/footer/paragraph

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

:docutils: decoration/footer/paragraph

:defaults:

     font-size: 12pt

     text-align: center

Formats the properties for the footer. Use to set the space
between the footer and the body text, using ``'space-before =
x'`` and setting ``'space-before.conditionality'`` to
``'retain'``.

TOC Matter Attribute Sets
=========================

Attribute sets for the TOC.

toc-block
---------

:fo: fo:block

:docutils: topic[@classes = "contents"]

:defaults:

Formats the block that wraps the TOC.

toc-title-block
---------------

:fo: fo:block

:docutils: topic[@classes = "contents"]/title

:defaults:

     text-align: center

     font-weight: bold

     font-size: 14pt

Formats the block for the title for the TOC.

toc-entry-defaults-block
------------------------

:fo: None

:docutils: None

:defaults:

     space-after: 3pt

     text-align-last: justify

Sets up the defaults for the TOC entries.

toc-level1-block
----------------

:fo: fo:block

:docutils: topic[@classes =
         "contents"]/bullet_list/list_item/paragraph/

:inherits: toc-entry-defaults-block

:defaults:

Formats the block for the level 1 table of contents entry. If a
number exists, it is formatted according to the parameter
'number-section1'.

toc-level2-block
----------------

:fo: fo:block

:docutils: topic[@classes =
         "contents"]/bullet_list/list_item/bullet_list/list_item/paragraph/

:inherits: toc-entry-defaults-block

:defaults:

     start-indent: 10mm

Formats the block for the level 2 table of contents entry. If a
number exists, it is formatted according to the parameter
'number-section2'.

toc-level3-block
----------------

:fo: fo:block

:docutils: topic[@classes =
         "contents"]/bullet_list/list_item/bullet_list/list_item/paragraph/etc.

:inherits: toc-entry-defaults-block

:defaults:

     start-indent: 20mm

Formats the block for the level 3 table of contents entry. If a
number exists, it is formatted according to the parameter
'number-section3'.

toc-level4-block
----------------

:fo: fo:block

:docutils: topic[@classes =
         "contents"]/bullet_list/list_item/bullet_list/list_item/paragraph/etc.

:inherits: toc-entry-defaults-block

:defaults:

     start-indent: 30mm

Formats the block for the level 4 table of contents entry. If a
number exists, it is formatted according to the parameter
'number-section4'.

toc-level5-block
----------------

:fo:

:docutils: topic[@classes =
         "contents"]/bullet_list/list_item/bullet_list/list_item/paragraph/etc.

:inherits: toc-entry-defaults-block

:defaults:

     start-indent: 40mm

Formats the block for the level 5 table of contents entry. If a
number exists, it is formatted according to the parameter
'number-section5'.

Section Attribute Sets
======================

Attribute sets for the section titles.

default-section-title-block
---------------------------

:fo: None

:docutils: None

:defaults:

     space-before: 12pt

     space-after: 12pt

     keep-with-next: always

Sets up the defaults for the section titles. The title should
always have some text beneath it to avoid widows and orphans;
hence the keep-with-always property.

title-level-block
-----------------

:fo: fo:block

:docutils: section/title|section/section/title|etc.

:inherits: default-section-title-block

The following attribute sets are identical in nature:

- title-level1-block

- title-level2-block

- title-level3-block

- title-level4-block

- title-level5-block

- title-level6-block

- title-level7-block

- title-level8-block

- title-level9-block

These attribute-sets format the titles of all sections.

title-number-inline
-------------------

:fo: fo:inline

:docutils: section/title/generated[@classes="sectnum]"

:defaults:

     space-end: 12pt

Formats the title number generated by docutils.

Body Elements
=============

Attribute sets for body elements, including the document title
and subtitle; the default paragraph; the transition element; and
the literal block.

paragraph-block
---------------

:fo: fo:block

:docutils: document/paragrah|section/paragraph"

:defaults:

     space-after: 12pt

Formats the default paragraph.

first-paragraph-block
---------------------

:fo: fo:block

:docutils: document/paragrah|section/paragraph"

:inherits: paragraph-block

:defaults:

Formats the first default paragraph.

literal-block
-------------

:fo: fo:block

:docutils: document/literal_block|section/literal_block"

:defaults:

     font-family: monospace

     font-size: 8

     white-space: pre

     space-after: 12pt

     space-before: 12pt

Formats the literal text.

transition-block
----------------

:fo: fo:block

:docutils: document/transition|section/transition"

:defaults:

     space-before: 12pt

     space-after: 12pt

     text-align: center

Formats the transition block. The actutal text for this block is
set by the 'transition-text' parameter.

document-title-block
--------------------

:fo: fo:block

:docutils: document/title"

:defaults:

     space-after: 12pt

     font-size: 24pt

     text-align: center

     font-weight: bold

Formats the title for the document.

document-title-page-block
-------------------------

:fo: fo:block

:docutils: document/title|document/subtitle"

:defaults:

The block that wraps both the title and subtitle. This block only
gets written if the title and subtitle occur in the front
section, or TOC section.

document-subtitle-block
-----------------------

:fo: fo:block

:docutils: document/subtitle"

:defaults:

     space-before: 12pt

     space-after: 12pt

     font-size: 18pt

     text-align: center

     font-weight: bold

Formats the subtitle of the document.

block-quote-outer-block
-----------------------

:fo: fo:block

:docutils: block_quote

:defaults:

     start-indent: 20mm

     end-indent: 20mm

     space-after: 12pt

     space-before: 12pt

The attribute set that formats the block that wraps the other
blocks in a block quote. Use the attribute set to format space
after or space before, etc.

block-quote-paragraph-block
---------------------------

:fo: fo:block

:docutils: block_quote/paragraph

:defaults:

     space-before: 12pt

The attribute set that formats the paragraphs in the block quote.
A different set of attributes controls the first paragraph (see
below). Use this attribute set to set the space between
paragraphs with the 'space-before' attribute.

block-quote-first-paragraph-block
---------------------------------

:fo: fo:block

:docutils: block_quote/paragraph[1]

:inherits: block-quote-paragraph-block

:defaults:

     space-before: 0pt

The attribute set that formats the first paragraph in the block
quote. It inherits all the attributes from
'block-quote-first-paragraph-block' and then sets the
'space-before' to 0. It does not make sense to modify attributes
in this attribute set directly.

block-quote-attribution-block
-----------------------------

:fo: fo:block

:docutils: block_quote/paragraph[1]

:inherits: block-quote-paragraph-block

:defaults:

     text-align: right

This attribute set the attribution in a block quote.

bullet list
===========

Attribute sets for the bullet list.

bullet-list-block
-----------------

:fo: list-block

:docutils: bullet_list

:defaults:

     start-indent: 5mm

     provisional-distance-between-starts: 5mm

     space-before: 12pt

     space-after: 12pt

For the bullet list. Since this element contains all the other
list elements, it can be used to set values such as the font,
background color, line-height, etc, for the entire list, as well
as the space after and before.

"The provisional-distance-between-starts property of the
list-block specifies the distance bewteen the start of the label
(the bullet, for example) and the actual start of the list
content" (Pawson, 100)

bullet-list-item
----------------

:fo: fo:list-item

:docutils: bullet_list/list_item

:defaults:

     space-before: 12pt

For the item in the bullet list. The attributes can control the
spacing between each item. A different set of attributes controls
the spacing of the first item (see below).

bullet-first-list-item
----------------------

:fo: fo:list-item

:docutils: bullet_list/list_item[1]

:inherits: bullet-list-item

:defaults:

     space-before: 0pt

For the first item in the bullet list. This attribute set
inherits all the properties form 'bullet-list-item', and then
re-defines the space-before to 0pt. In order to get space between
the first item and the text before it, use the space-after
attribute in the bullet-list attribute set.

bullet-level2-list-item
-----------------------

:fo: fo:list-item

:docutils: bullet_list/bullet_list/list_item

:defaults:

     space-before: 12pt

Same as above, except for a nested bullet list.

bullet-level2-first-list-item
-----------------------------

:fo: fo:list-item

:docutils: bullet_list/list_item[1]

:inherits: bullet-level2-list-item

:defaults:

     space-before: 0pt

For the first item in a nested bullet list. This attribute set
inherits all the properties form 'bullet-list-item', and then
re-defines the space-before to 0pt. In order to get space between
the first item and the text before it, use the space-after
attribute in the bullet-list attribute set.

bullet-list-item-label
----------------------

:fo: fo:list-item-label

:docutils: bullet_list/list_item

:defaults:

     end-indent: label-end()

The default attribute end-indent = "label-end()" ensures that the
label aligns properly.

bullet-list-item-label-block
----------------------------

:fo: fo:block

:docutils: bullet_list/list_item

:defaults:

These attributes format the block that wraps the bullet. (FO
requires such a block, even for a small label like this.)

bullet-list-item-body
---------------------

:fo: fo:list-item-body

:docutils: bullet_list/list_item

:defaults:

     start-indent: body-start()

The default of start-indent = "body-start()" ensures the correct
alignment of the labels.

bullet-list-item-body-block
---------------------------

:fo: fo:block

:docutils: bullet_list/list_item/paragraph

:defaults:

     space-after: 12pt

Formats the blocks (docutilis paragraphs) of the body of each
item.

bullet-level2-list-block
------------------------

:fo: list-block

:docutils: bullet_list/bullet_list

:defaults:

     start-indent: 15mm

     provisional-distance-between-starts: 5mm

     space-before: 12pt

Same as for the bullet-list-block attribute. The default sets the
start-indent property to a greater value to indent this nested
list.

enumerated list
===============

Attribute sets for the enumerated list.

enumerated-list-block
---------------------

:fo: list-block

:docutils: enumerated_list

:defaults:

     start-indent: 5mm

     provisional-distance-between-starts: 10mm

     space-before: 12pt

     space-after: 12pt

For the enumerated list. Since this element contains all the
other list elements, it can be used to set values such as the
font, background color, line-height, etc, for the entire list, as
well as the space after and before.

"The provisional-distance-between-starts property of the
list-block specifies the distance bewteen the start of the label
(the bullet, for example) and the actual start of the list
content" (Pawson, 100)

enumerated-level2-list-block
----------------------------

:fo: list-block

:docutils: enumerated_list/enumerated_list

:defaults:

     start-indent: 15mm

     provisional-distance-between-starts: 10mm

     space-before: 12pt

     space-before: 12pt

Same as for the enumerated-list-block attribute. The default sets
the start-indent property to a greater value to indent this
nested list.

enumerated-list-item
--------------------

:fo: fo:list-item

:docutils: enumerated_list/list_item

:defaults:

     space-before: 12pt

For the item in the enumerated list. The attributes can control
the spacing between each item. A different set of attributes
controls the spacing of the first item (see below).

enumerated-first-list-item
--------------------------

:fo: fo:list-item

:docutils: enumerated_list/list_item[1]

:inherits: enumerated-list-item

:defaults:

     space-before: 0pt

For the first item in the enumerated list. This attribute set
inherits all the properties form 'enumerated-list-item', and then
re-defines the space-before to 0pt. In order to get space
between the first item and the text before it, use the
space-after attribute in the enumerated-list attribute set.

enumerated-level2-list-item
---------------------------

:fo: fo:list-item

:docutils: enumerated_list/list_item/enumerated_list/list-item

:defaults:

     space-before: 12pt

Same as above, but formats item of nested list.

enumerated-level2-first-list-item
---------------------------------

:fo: fo:list-item

:docutils: enumerated_list/item/enumerated_list/list_item[1]

:inherits: enumerated-level2-list-item

:defaults:

     space-before: 0pt

For the first item in the nested enumerated list.

enumerated-list-item-label
--------------------------

:fo: fo:list-item-label

:docutils: enumerated_list/list_item

:defaults:

     end-indent: label-end()

The default attribute end-indent = "label-end()" ensures that the
label aligns properly.

enumerated-list-item-body
-------------------------

:fo: fo:list-item-body

:docutils: enumerated_list/list_item

:defaults:

     start-indent: body-start()

The default of start-indent = "body-start()" ensures the correct
alignment of the labels.

enumerated-list-item-body-block
-------------------------------

:fo: fo:block

:docutils: enumerated_list/list_item/paragraph

:defaults:

     space-after: 12pt

Formats the blocks (docutilis paragraphs) of the body of each
item.

definition list
===============

Attribute sets for the definition list.

definition-list-block
---------------------

:fo: block

:docutils: definition_list

:defaults:

     space-after: 12pt

     space-before: 12pt

For the definition list. Since this element contains all the
other blocks in the list, it can be used to set values such as
the font, background color, line-height, etc, for the entire
list, as well as the space after and before.

definition-list-item-block
--------------------------

:fo: fo:block

:docutils: definition_list/definition_list_item

:defaults:

     space-before: 12pt

For the items in the definition list. The attributes can control
the spacing between each item. A different set of attributes
controls the spacing of the first item (see below).

definition-list-item-first-block
--------------------------------

:fo: fo:block

:docutils: definition_list/definition_list_item

:inherits: definition-list-item-block

:defaults:

     space-before: 0pt

For the first item in the definition list. This attribute set
inherits all the properties form 'definition-list-item', and then
re-defines the space-before to 0pt. In order to get space
between the first item and the text before it, use the
space-after attribute in the option-list attribute set.

It does not makes sense to change this set direclty.

definition-term-block
---------------------

:fo: fo:block

:docutils: definition_list/definition_list_item/term

:defaults:

     font-weight: bold

Formats the bock of the the term. Can be used to control spacing
between term and definition, but don't use with space before, or
you won't be able to control spacing before list

definition-block
----------------

:fo: fo:block

:docutils: definition_list/definition_list_item/definition

:defaults:

Formats the bock of the of the defintion, that wraps the
paragraph blocks.

classifier-inline
-----------------

:fo: fo:inline

:docutils: definition_list/definition_list_item/classifier

:defaults:

     font-style: italic

For the inine properties of the classifier item.

definition-paragraph-block
--------------------------

:fo: fo:block

:docutils:
         definition_list/definition_list_item/definition/paragraph

:defaults:

     space-before: 12pt

     start-indent: 30pt

Formats the blocks (paragraphs in the defintion. Can be lsed to
control the space between paragraphs by setting the space-bfore
attribute. Don't use the space-after attribute, or you won't be
able to contorl the spacing between items

definition-first-paragraph-block
--------------------------------

:fo: fo:block

:docutils:
         definition_list/definition_list_item/definition/paragraph[1]

:inherits: definition-first-paragraph-block

:defaults:

     space-before: 0pt

For the first paragraph in the definition list. This attribute
set inherits all the properties frorm
'definition-first-paragraph-block', and then re-defines the
space-before to 0pt.

It does not makes sense to change this set direclty.

field list
==========

Attribute sets for the field list.

field-list-block
----------------

:fo: list-block

:docutils: field_list

:defaults:

     start-indent: 0mm

     provisional-distance-between-starts: 30mm

     space-before: 12pt

     space-after: 12pt

Formats the field list. Since this element contains all the other
list elements, it can be used to set values such as the font,
background color, line-height, etc, for the entire list, as well
as the space after and before.

"The provisional-distance-between-starts property of the
list-block specifies the distance bewteen the start of the label
(the bullet, for example) and the actual start of the list
content" (Pawson, 100).

field-list-item
---------------

:fo: fo:list-item

:docutils: field_list/field

:defaults:

     space-before: 12pt

For the items, or 'fields' in the field list. The attributes can
control the spacing between each item. A different set of
attributes controls the spacing of the first item (see below).

field-first-list-item
---------------------

:fo: fo:list-item

:docutils: field_list/field[1]

:inherits: field-list-item

:defaults:

     space-before: 0pt

For the first item in the field list. This attribute set inherits
all the properties form 'field-list-item', and then re-defines
the space-before to 0pt. In order to get space between the first
item and the text before it, use the space-after attribute in the
field-list-block attribute set.

It does not make sense to change this attriubte set directly.

field-list-item-label
---------------------

:fo: fo:list-item-label

:docutils: field_list/field/field_name

:defaults:

     end-indent: label-end()

The default attribute end-indent = "label-end()" ensures that the
label aligns properly.

field-list-item-body
--------------------

:fo: fo:list-item-body

:docutils: field_list/field/field_body

:defaults:

     start-indent: body-start()

The default of start-indent = "body-start()" ensures the correct
alignment of the labels.

field-body-block
----------------

:fo: fo:block

:docutils: field_list/field/field_body/paragraph

:defaults:

     space-after: 12pt

Formats the blocks (docutilis paragraphs) of the field.

field-list-item-label-block
---------------------------

:fo: fo:block

:docutils: field_list/field/field_name

:defaults:

     font-weight: bold

Formats the block that wraps the field name.

option list as list
===================

Since an option list can be rendered as either a traditonal list,
or a definition list, there are two sets of attribute sets.
These attribute sets are used for the options list when it is
rendered as a list.

option-list-block
-----------------

:fo: list-block

:docutils: option_list

:defaults:

     start-indent: 0mm

     provisional-distance-between-starts: 50mm

     space-before: 12pt

     space-after: 12pt

For the option list. Since this element contains all the other
list elements, it can be used to set values such as the font,
background color, line-height, etc, for the entire list, as well
as the space after and before.

"The provisional-distance-between-starts property of the
list-block specifies the distance bewteen the start of the label
(the bullet, for example) and the actual start of the list
content" (Pawson, 100)

option-list-item
----------------

:fo: fo:list-item

:docutils: option_list/option_list_item

:defaults:

     space-before: 12pt

For the items in the option list. The attributes can control the
spacing between each item. A different set of attributes controls
the spacing of the first item (see below).

option-first-list-item
----------------------

:fo: fo:list-item

:docutils: option_list/option_list_item[1]

:inherits: option-list-item

:defaults:

     space-before: 0pt

For the first item in the option list. This attribute set
inherits all the properties form 'option-list-item', and then
re-defines the space-before to 0pt. In order to get space between
the first item and the text before it, use the space-after
attribute in the option-list attribute set.

It does not make sense to change this attriubte set directly.

option-list-item-label
----------------------

:fo: fo:list-item-label

:docutils: option_list/option_list_item/option_group

:defaults:

     end-indent: label-end()

The default attribute end-indent = "label-end()" ensures that the
label aligns properly.

option-list-item-label-block
----------------------------

:fo: fo:block

:docutils:
         option_list/option_list_item/option_group/option_string|option_argument

:defaults:

These attributes format the block that wraps the option_string
and option_argument.

option-list-item-body
---------------------

:fo: fo:list-item-body

:docutils: option_list/option_list_item/description

:defaults:

     start-indent: body-start()

The default of start-indent = "body-start()" ensures the correct
alignment of the labels.

option-list-item-body-block
---------------------------

:fo: fo:block

:docutils: option_list/option_list_item/description/paragraph

:defaults:

Formats the blocks (docutilis paragraphs) that describe the
options. If there was more than one paragraph, you could use
attributes such as space after.

option-inline
-------------

:fo: fo:inline

:docutils:
         option_list/option_list_item/option_group/option/option_string

:defaults:

     font-family: monospace

Used to format any inline properties of the option_string.

option-argument-inline
----------------------

:fo: fo:inline

:docutils:
         option_list/option_list_item/option_group/option/option_argument

:defaults:

     font-family: monospace

     font-style: italic

Used to format any inline properties of the option_string.

option list as definition list
==============================

These attribute sets are used for the options list when it is
rendered as a definition list. (See the docutils reference guide
for an example of a definition list, or see the defintion list in
the test files.)

option-list-definition-block
----------------------------

:fo: fo:block

:docutils: option_list

:defaults:

     space-before: 12pt

     space-after: 12pt

Formats the block that wraps the other blocks. Use to control
space after and before, or to set any block items on the entire
list.

This block wraps around another block, which in turn wraps around
a third block.

option-list-item-block
----------------------

:fo: fo:block

:docutils: option_list/option_list_item

:defaults:

     space-before: 8pt

Formats the block that wraps the options and descriptions, which
are also blocks.

option-list-first-item-block
----------------------------

:fo: fo:block

:docutils: option_list/option_list_item

:defaults:

     space-before: 0pt

Same as for option-list-item-block, but sets the space-before to
0pt

Does not make sense to change the attributes here directly.

option-group-block
------------------

:fo: fo:block

:docutils: option_list_item/option_list_item/option_group

:defaults:

     keep-with-next: always

Formats the block that contains the inline elements of the
options and arguments. For a defintion list, this block serves as
the term, and sits on top, and to the left of the description.

option-list-description-block
-----------------------------

:fo: fo:block

:docutils: option_list/option_list_item/description

:defaults:

     start-indent: 16pt

     space-before: 8pt

Formats the blocks wrappring the paragraphs describing the
options or arguments. This groups of blocks sits below the blocks
formatting the options and arguments, and in a defintion list
are usually indented right.

option-list-paragraph-block
---------------------------

:fo: fo:block

:docutils: option_list/option_list_item/description/paragraph

:defaults:

     space-before: 0pt

Formats the paragraphs in the description for an options list
formatted as a definition list.

option-list-first-paragraph-block
---------------------------------

:fo: fo:block

:docutils: option_list/option_list_item/description/paragraph

:inherits: option-list-paragraph-block

:defaults:

     space-before: 0pt

Formats the first paragraph in the description for an options
list formatted as a definition list.

Line Block
==========

Attribute sets for the line block.

outer-line-block
----------------

:fo: fo:block

:docutils: line_block

:defaults:

     space-before: 12pt

     space-after: 12pt

The outer block containing the blocks of lines. Use the outer
block to set space before or after the verse.

level1-line-block
-----------------

:fo: fo:block

:docutils: line_block/line

:defaults:

     start-indent: 10mm

Attribute sets for the first level of lines.

level2-line-block
-----------------

:fo: fo:block

:docutils: line_block/line_block/line

:defaults:

     start-indent: 20mm

Attribute sets for the second level of lines.

level3-line-block
-----------------

:fo: fo:block

:docutils: line_block/line_block/line_block/line

:defaults:

     start-indent: 30mm

Attribute sets for the third level of lines.

level4-line-block
-----------------

:fo: fo:block

:docutils: line_block/line_block/line_block/line_block/line

:defaults:

     start-indent: 40mm

Attribute sets for the fourth level of lines.

level5-line-block
-----------------

:fo: fo:block

:docutils:
         line_block/line_block/line_block/line_block/line_block/line

:defaults:

     start-indent: 50mm

Attribute sets for the fifth level of lines.

stanza-title-block
------------------

:fo: fo:block

:docutils: line_block/title_reference

:defaults:

     text-align: center

     space-before: 12

     font-weight: bold

Formats the title of a stanza.

Table
=====

Attribute sets for the Table.

table-block-container
---------------------

:fo: fo:block-container

:docutils: table

:defaults:

     space-before: 12pt

     space-after: 12pt

Formats the block container that wraps bothe the table and a the
table title (captin) if one exists. Use to control space before
and after the table.

table
-----

:fo: fo:table

:docutils: table

:defaults:

     table-layout: fixed

     inline-progression-dimension: 100%

Formats the table.

table-header
------------

:fo: fo:table-header

:docutils: tgroup/thead

:defaults:

     font-weight: bold

Formats the header of the table.

default-cell
------------

:fo: fo:cell

:docutils: None

:defaults:

     border: solid black 1px

     padding: 1em

     border-collapse: collapse

Sets the defaults for all cells.

table-header-cell
-----------------

:fo: fo:cell

:docutils: thead/row/entry

:inherits: default-cell

:defaults:

     border-bottom: solid black 2px

Formats the cells in the table header.

table-header-block
------------------

:fo: fo:block

:docutils: thead/row/entry/paragraph

:defaults:

Attributes for the paragraphs in the header cell.

table-body
----------

:fo: fo:table-body

:docutils: tbody

:defaults:

Attributes for the table body.

table-row
---------

:fo: fo:table-row

:docutils: tbody/row

:defaults:

     keep-together.within-page: always

Attributes for the table row.

table-cell
----------

:fo: fo:table-cell

:docutils: tbody/row/entry

:inherits: default-cell

:defaults:

Attributes for the table cell.

cell-block
----------

:fo: fo:block

:docutils: tbody/row/entry/paragraph

:defaults:

Attributes for the paragraphs in the cell.

caption-block
-------------

:fo: fo:block

:docutils: table/title

:defaults:

     text-align: center

     space-before: 6pt

     space-after: 6pt

Attributes for the table title, or caption. The parameter
'table-title-placement', controls whether this block is placed
before or after the table. If it is placed on top of the table,
it has a 'keep-with-next="always"' value that cannot be changed.
If this block is placed on the bottom it has a
'keep-with-previous="always"' value that cannot be changed.

Table Extended
==============

Attribute sets for the Extended Tables.

Custom Tables
-------------

:fo: See below

:docutils: See below

The table_extend.xsl contains templates to match 30 custom
tables, and and attribute sets for each of these tables. Each
attribute set has has the same function as its corresponding
attribute set in the regular table, as documented above.

* table1-block-container => table-block-contanter

* table1 => table

* table1-header => table-header

* table1-header-cell => table-header-cell

* table1-header-block => table-header-block

* table1-body => table-body

* table1-header-row => table-header-row

* table1-row => table-row

* table1-cell => table-cell

* cell1-block => cell-block

* table2-block-container => table-block-contanter

* table2 => table

* ...

* table2-row => table-row

* table2-cell => table-cell

* cell2-block => cell-block

* ...

* table30-block-container => table-block-contanter

* table30 => table

* table30-header => table-header

* table30-header-cell => table-header-cell

* table30-header-block => table-header-block

* table30-body => table-body

* table30-header-row => table-header-row

* table30-row => table-row

* table30-cell => table-cell

* cell30-block => cell-block

Table Extended 2 (Borderless)
=============================

Attribute sets for the Extended 2 (Borderless) Tables.

Custom Tables
-------------

:fo: See below

:docutils: See below

The table_extend2.xsl contains templates for the borderless
table. Each attribute set has has the same function as its
corresponding attribute set in the regular table, as documented
above.

* borderless-table-block-container => table-block-contanter

* borderless-table => table

* borderless-table-header => table-header

* borderless-table-header-cell => table-header-cell

* borderless-table-header-block => table-header-block

* borderless-table-body => table-body

* borderless-table-header-row => table-header-row

* borderless-table-row => table-row

* borderless-table-cell => table-cell

* borderless-cell-block => cell-block

Table Long
==========

Attribute sets for the Long Tables.

Long Tables
-----------

:fo: See below

:docutils: See below

The table_long.xsl contains templates for a long table. Each
attribute set has has the same function as its corresponding
attribute set in the regular table, as documented above. There is
no block-container attriute set, because there is no
block-container element; the caption is rendered as part of the
header or footer.

* long-table => table

* long-table-header => table-header

* long-table-header-row => table-header-row

* long-thead-cell => thead-cell

* long-table-header-block => table-header-block

* long-table-body => table-body

* long-table-header-row => table-header-row

* long-table-row => table-row

* long-table-cell => table-cell

* long-cell-block => cell-block

* long-caption-block => caption-block

Footnote
========

Attribute sets for footnotes, endnotes, and the endnotes title.

footnote
--------

:fo: fo:footnote

:docutils: footnote

:defaults:

     font-weight: normal

     font-style: normal

Formats the footnote. By default, it sets properties to neutral,
so that it does not inherit any unwanted properties, such as from
a definition term.

default-footnote-label-inline
-----------------------------

:fo: fo:inline

:docutils: None

:defaults:

     baseline-shift: super

     font-size: 8pt

Sets of the defaults for the label (1, \*, etc), of each label.

footnote-label-inline
---------------------

:fo: fo:inline

:docutils: footnote/paragraph[1]

:inherits: default-footnote-label-inline

:defaults:

Formats the label for *traditional* footnotes and endnotes at the
bottomm of the page or with the endnotes. This attribute set
does not affect the label for footnotes and endnotes formatted as
a list.

footnote-body-label-inline
--------------------------

:fo: fo:inline

:docutils: footnote/paragraph[1]

:inherits: default-footnote-label-inline

:defaults:

Formats the label for *traditional* footnotes and endnotes in the
body of the text. This attribute set does not affect the label
for footnotes and endnotes formatted as a list.

footnote-list-block
-------------------

:fo: fo:list-block

:docutils: footnote

:defaults:

     provisional-label-separation: 0pt

     provisional-distance-between-starts: 18pt

Formats the list that contains the footnote. The
'provisional-distance-between-starts' controls how far away the
footnote label is from the text.

footnote-item-label
-------------------

:fo: fo:list-item-label

:docutils: footnote

:defaults:

     end-indent: label-end()

Formats the item-label when the footnote or endnote is formatted
as a list.

footnote-label-block
--------------------

:fo: fo:block

:docutils: footnote_reference

:defaults:

Formats the block in item-label when the footnote or endnote is
formatted as a list. By default, the label has no superscript (as
opposed to when formatting a "traditional" footnote.

footnote-item-body
------------------

:fo: fo:list-item-body

:docutils: footnote

:defaults:

     start-indent: body-start()

Formats the item-body when the footnote or endnote is formatted
as a list.

footnote-body
-------------

:fo: fo:footnote-body

:docutils: footnote

:defaults:

Formats the body of the footnote. Space-after and space-before
seem to have no affect, at least with fop.

footnote-paragraph-block
------------------------

:fo: fo:block

:docutils: footnote/paragraph

:defaults:

     space-before: 5pt

Formats the paragraphs in the body of a footnote or endnote. Use
the 'space-before' to set the space between each paragraphs, for
footnotes or endnotes with multiple paragraphs. In addition, for
traditional footnotes, use the 'text-indent="18pt" to create a
traditional footnote. (The deault does not do this, in order to
accommodate the footnote-as-a-list.)

footnote-first-paragraph-block
------------------------------

:fo: fo:block

:docutils: footnote/paragraph[1]

:inherits: footnote-paragraph-block

:defaults:

     space-before: 0pt

Formats the first paragraphs in the body of a footnote or
endnote. It inherits all the attributes from the
'footnote-paragraphs-block' and sets the space-before to 0. It
does not make sense to change attributes on this block directly.

endnotes-block
--------------

:fo: fo:block

:docutils: footnote

:defaults:

     break-before: page

The block that wraps all the other blocks of the endnotes. Use to
create a page break before, or to create space before and after
the endnotes.

endnote-block
-------------

:fo: fo:block

:docutils: footnote

:defaults:

     space-before: 5pt

The block that wraps each individual endnote ('footnote' in
docutils). Use to control the spacing between each endnote.

endnote-first-block
-------------------

:fo: fo:block

:docutils: footnote

:inherits: endnote-block

:defaults:

     space-before: 0pt

The block that wraps each the first endnote ('footnote' in
docutils). It does not make sense to change attributes on this
set directly.

endnotes-title-block
--------------------

:fo: fo:block

:docutils: rubric[@classes='endnotes']

:defaults:

     space-after: 18pt

     font-weight: bold

     font-size: 18pt

     text-align: center

Formats the title for the endnotes, when one is present.

Admonitions
===========

Attribute sets for Admonitions. By default, the admontioins have
a border around them. Each admonition gets its title from the
parameter of that name. For example, the danger admonitions title
gets its title from the 'danger-title' parameter, the caution
from the `caution-title` paramter, and so fourth.

Although each admonition and each admonition title has its own
attribute-set, by default they all inherit these values from two
default attribute sets. (See below.) Each of these areas can thus
be customized. In contrast, all the paragrahs in each admonition
are identical.

default-admonition-outer-block
------------------------------

:fo: block

:docutils: None

:defaults:

     border-style: solid

     border-width: 1px

     padding: 6pt

     keep-together.within-page: always

Sets up the defaults for the outer blocks of all the admonitions.
The attributes of this block control the borders and prohibit
the admonition from breaking across a page.

default-admonition-title-block
------------------------------

:fo: block

:docutils: None

:defaults:

     space-after: 10pt

     font-size: larger

     color: red

Sets up the defaults for the title blocks of all the admonitions.
The attributes of this block control the color (red) and font
size. For certain blocs, the color is set to black (see below).

admonitions outer block
-----------------------

:fo: fo:block

:docutils: attention | caution | danger | error | hint |
         important | note | tip | warning |
         admonitons[@classes='custorm']

:inherits: default-admonition-outer-block

The following attribute sets are identical in nature:

* attention-block

* caution-block

* danger-block

* error-block

* hint-block

* important-block

* note-block

* tip-block

* warning-block

* admonition-custom-block

These attribute-sets format the outer block of all the
admonitions. By default it puts an border around the text. Use
this attribute set to set the space before or after, the
background color, etc.

admonitions title block
-----------------------

:fo: fo:block

:docutils: attention | caution | danger | error | hint |
         important | note | tip | warning |
         admonitons[@classes='custorm']

:inherits: default-admonition-title-block

The following attribute sets are identical in nature:

* attention-title-block

* caution-title-block

* danger-title-block

* error-title-block

* hint-title-block

* important-title-block

* note-title-block

* tip-title-block

* warning-title-block

* admonition-custom-title-block

These attribute-sets format the title block of all the
admonitions. It sets the color to red.

The attribute-sets ``error-title-block``, ``hint-title-block``,
``important-title-block``, ``note-title-block``,
``tip-title-block``, and ``admonition-custom-title-block`` resets
the color back to black.

admonition-paragraph-block
--------------------------

:fo: fo:block

:docutils: attention/paragraph|caution/paragraph|etc.

:defaults:

     space-before: 12pt

Formats the paragraphs in the admonitions. A different
attribute-set formats the first paragraph (see below).

admonition-first-paragraph-block
--------------------------------

:fo: fo:block

:docutils: attention/paragraph[1]|caution/paragraph[1]|etc.

:defaults:

Formats the first paragraphs in the admonitions. It inherits its
attributes from the ``admonition-paragraph-block`` and resets the
``space-before`` property to ``0pt``. It does not make sense to
modify the attributes in this set directly.

Image and Figure
================

Attribute sets for Images and Figures. The image property of
``alt`` and ``target`` are ignored by the stylesheets, since they
have no use in PDF. In addtion, if the ``align`` is set to
``top`` or ``bottom``, both properties that have no meaning for
PDF, the stylesheets will report an error, and if ``strict`` is
set to ``true``, quit.

Likwise, the figure ``figwidth`` property will be ignored, since
there is not way to implement this property directy in FO.

In order to control the scaling, alignment, and width of images
and figures, it is better to use the attribute sets than to try
to set these properties in RST. The regions of 'image', 'figure',
'caption', and 'legend' are all wrapped in blocks. Use the
attribute sets for these blocks to control the properties.

figure-block
------------

:fo: fo:block

:docutils: figure

:defaults:

Formats the block that wraps the figure. Use this attribute set
to set properties on the image, caption, and legend, as well as
to set the space before and after the figure.

image-block
-----------

:fo: fo:block

:docutils: image

:defaults:

Formats the block that wraps the image, both for an image by
itself, and for an image included in a figure. Use this attribute
set to control the space before and after the image, as well as
to align the image itself.

figure-caption-block
--------------------

:fo: fo:block

:docutils: figure/caption

:defaults:

     space-before: 12pt

     space-after: 12pt

     font-weight: bold

     font-size: smaller

     text-align: center

Formats the block that wraps the caption.

figure-legend-block
-------------------

:fo: fo:block

:docutils: figure/legend

:defaults:

     space-before: 12pt

     space-after: 12pt

Formats the block that wraps the legend. The paragrahs in the
legend have their own blocks.

legend-paragraph-block
----------------------

:fo: fo:block

:docutils: figure/legend/paragaph

:defaults:

     space-before: 12pt

Formats the block that wraps the paragraphs in the legend.

legend-first-paragraph-block
----------------------------

:fo: fo:block

:docutils: figure/legend/paragaph[1]

:inherits: legend-paragraph-block

:defaults:

     space-before: 0pt

Formats the first block that wraps the paragraphs in the legend.

Body Elements Directives
========================

Attribute sets for Body Elements Directives.

topic-block
-----------

:fo: fo:block

:docutils: topic

:defaults:

     space-after: 12pt

     space-before: 12pt

Formats the outermost block of the topic element, which contains
blocks.

topic-title-block
-----------------

:fo: fo:block

:docutils: topic/title

:defaults:

     font-weight: bold

     space-after: 12pt

Formats the title of the topic.

topic-paragraph-block
---------------------

:fo: fo:block

:docutils: topic/paragraph

:defaults:

     space-before: 12pt

     space-after: 0pt

Formats the paragraphs of the topic. A different set of
attributes formats the first paragraph.

topic-first-paragraph-block
---------------------------

:fo: fo:block

:docutils: topic/paragraph[1]

:inherits: topic-paragraph-block

:defaults:

Formats the first paragraphs of the topic.

sidebar-block
-------------

:fo: fo:block

:docutils: sidebar

:defaults:

     space-after: 12pt

     space-before: 12pt

     background-color: #FFFFF0

     padding: 6pt

     start-indent: 10mm

     end-indent: 40mm

Formats the outermost block of the sidebar element, which
contains blocks. Note that fop does not handle floats, so this
element is formatted just like a topic block.

sidebar-title-block
-------------------

:fo: fo:block

:docutils: sidebar/title

:defaults:

     font-weight: bold

     space-after: 12pt

Formats the title of the topic.

sidebar-subtitle-block
----------------------

:fo: fo:block

:docutils: sidebar/subtitle

:defaults:

     font-weight: bold

     space-after: 12pt

Formats the subtitle of the topic.

sidebar-paragraph-block
-----------------------

:fo: fo:block

:docutils: sidebar/paragraph

:defaults:

     space-before: 12pt

Formats the paragraphs of the sidebar. A different set of
attributes formats the first paragraph.

sidebar-first-paragraph-block
-----------------------------

:fo: fo:block

:docutils: sidebar/paragraph[1]

:inherits: sidebar-paragraph-block

:defaults:

     space-after: 0pt

Formats the first paragraphs of the sidebar.

rubric-block
------------

:fo: fo:block

:docutils: rubric

:defaults:

     text-align: center

     font-size: larger

     color: red

Formats the rubric.

epigraph-outer-block
--------------------

:fo: fo:block

:docutils: epigraph

:defaults:

     start-indent: 20mm

     end-indent: 20mm

     space-after: 12pt

     space-before: 12pt

     text-align: right

     font-style: italic

Formats the outermost block of the epigraph element, which
contains blocks.

epigraph-paragraph-block
------------------------

:fo: fo:block

:docutils: epigraph/paragraph

:defaults:

     start-indent: inherit

     end-indent: inherit

     space-before: 12pt

Formats the paragraphs of the epigraph. A different set of
attributes formats the first paragraph.

epigraph-first-paragraph-block
------------------------------

:fo: fo:block

:docutils: epigraph/paragraph[1]

:inherits: epigraph-paragraph-block

:defaults:

     space-before: 0pt

Formats the first paragraphs of the epigraph.

epigraph-attribution-block
--------------------------

:fo: fo:block

:docutils: epigraph/attribution

:defaults:

     text-align: right

Formats the attribution of the epigraph. The parameter
``text-before-epigraph-attribution`` determines the text to put
before the attribtion. The default is '—' (an em-dash). To put no
text before, set this parameter to an empty string.

highlights-outer-block
----------------------

:fo: fo:block

:docutils: highlights

:defaults:

     start-indent: 20mm

     end-indent: 20mm

     space-after: 12pt

     space-before: 12pt

Formats the outermost block of the epigraph element, which
contains blocks.

highlights-paragraph-block
--------------------------

:fo: fo:block

:docutils: highlights/paragraph

:defaults:

     start-indent: inherit

     end-indent: inherit

     space-before: 12pt

Formats the paragraphs of the highlights. A different set of
attributes formats the first paragraph.

highlights-first-paragraph-block
--------------------------------

:fo: fo:block

:docutils: highlights/paragraph[1]

:inherits: highlights-paragraph-block

:defaults:

     space-before: 0pt

Formats the first paragraphs of the highlights.

pull-quote-outer-block
----------------------

:fo: fo:block

:docutils: pull-quote

:defaults:

     start-indent: 20mm

     end-indent: 20mm

     space-after: 12pt

     space-before: 12pt

Formats the outermost block of the pull-quote element, which
contains blocks.

pull-quote-paragraph-block
--------------------------

:fo: fo:block

:docutils: pull-quote/paragraph

:defaults:

     start-indent: inherit

     end-indent: inherit

     space-before: 12pt

Formats the paragraphs of the pull-quote. A different set of
attributes formats the first paragraph.

pull-quote-first-paragraph-block
--------------------------------

:fo: fo:block

:docutils: pull-quote/paragraph[1]

:inherits: pull-quote-paragraph-block

:defaults:

     space-before: 0pt

Formats the first paragraphs of the pull-quote.

pull-quote-attribution-block
----------------------------

:fo: fo:block

:docutils: pull-quote/attribution

:defaults:

     text-align: right

Formats the attribution of the pull-quote. The parameter
``text-before-pull-quote-attribution`` determines the text to put
before the attribtion. The default is '—' (an em-dash). To put
no text before, set this parameter to an empty string.

container-outer-block
---------------------

:fo: fo:block

:docutils: container

:defaults:

     space-after: 12pt

     space-before: 12pt

Formats the outermost block of the container element, which
contains blocks.

container-paragraph-block
-------------------------

:fo: fo:block

:docutils: container/paragraph

:defaults:

     space-before: 12pt

Formats the paragraphs of the container. A different set of
attributes formats the first paragraph.

container-first-paragraph-block
-------------------------------

:fo: fo:block

:docutils: container/paragraph[1]

:inherits: container-paragraph-block

:defaults:

     space-before: 0pt

Formats the first paragraphs of the container.

Inline
======

Attribute sets for all the inline elements. The parameter
'footnote-style' controls the style of the footnote. The
paramater 'footnote-placement' determines whether the footnotes
that are numbered will be placed as footnotes or endnotes.

The parameter 'space-between-foototes' controls the space between
footnotes. (Becuase of a flaw(?) in FOP, an attribute set could
not be used.) This parameter has no effect on the space between
endnotes.

emphasis-inline
---------------

:fo: fo:inline

:docutils: emphasis

:defaults:

     font-style: italic

Formats the emphasis element.

strong-inline
-------------

:fo: fo:inline

:docutils: strong

:defaults:

     font-weight: bold

Formats the strong element.

basic-link-inline
-----------------

:fo: fo:inline

:docutils: basic_link

:defaults:

     text-decoration: underline

     color: blue

Formats the basic_link element.

literal-inline
--------------

:fo: fo:inline

:docutils: literal

:defaults:

     font-family: monospace

     font-size: 8

     white-space: pre

Formats the literal element.

title-reference-inline
----------------------

:fo: fo:inline

:docutils: title-reference

:defaults:

     font-style: italic

Formats the title_reference element.

