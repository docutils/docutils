Page Layout
=============

Paper Size
------------

The following determmine the size of the paper:

Height
+++++++

* default-simple-page-master.page-height = 11in

The short version is:

* paper-size.height = 11in

Width
++++++

* default-simple-page-master.page-width = 8.5in

The short version is:

* paper-size.width = 8.5in

Page Margins
---------------

For a Simple Document
+++++++++++++++++++++++

Each set of properties has the same effect.

* simple-page-master.top-margin = 1in
* simple-page-master.bottom-margin = 1in
* simple-page-master.right-margin = 1in
* simple-page-master.left-margin = 2in

The short versions are:

* page.top-margin = .75in
* page.bottom-margin = .75in
* page.right-margin = .75in
* page.left-margin = .75in

The properties can be reversed:
 
* page.margin-top = .75in
* page.margin-bottom = .75in
* page.margin-right = .75in
* page.margin-left = .75in


For Documents with First, Odd, and Even Pages
++++++++++++++++++++++++++++++++++++++++++++++

Here is how to set margins for the first, even, and odd pages. Note that these
will work only if you choose ``page-layout``=``odd-even``, or ``page-layout``
= ``first-odd-even,`` or ``page-layout`` = ``first``.

The long version:

* odd-simple-page-master.top-margin = 1in
* odd-simple-page-master.bottom-margin = 1in
* odd-simple-page-master.right-margin = 1in
* odd-simple-page-master.left-margin = 2in
* 
* even-simple-page-master.top-margin = 1in
* even-simple-page-master.bottom-margin = 1in
* even-simple-page-master.right-margin = 2in
* even-simple-page-master.left-margin = 1in

* first-simple-page-master.top-margin = 1in
* first-simple-page-master.bottom-margin = 1in
* first-simple-page-master.right-margin = 2in
* first-simple-page-master.left-margin = 1in

The short versions:

* odd-page.top-margin = 1in
* odd-page.bottom-margin = 1in
* odd-page.right-margin = 1in
* odd-page.left-margin = 2in
* 
* even-page.top-margin = 1in
* even-page.bottom-margin = 1in
* even-page.right-margin = 2in
* even-page.left-margin = 1in

* first-page.top-margin = 3in
* first-page.bottom-margin = 1in
* first-page.right-margin = .8in
* first-page.left-margin = 2in

Headers and Footers
---------------------

Setting the space, or height.
++++++++++++++++++++++++++++++

Long version:

* header-region-before.extent = 1in
* footer-region-after.extent = 1in

Short version:

* header.height = 1in
* footer.height = 1in

To make the header closer to the page, use the ``space-before`` property::

 header.space-before = .2in

Use the ``'space-before'`` property for the footer, as well, to move it
closer to the  text::

 footer.space-before = -.1in

Documnet
=============

The following properties can be set for the entire document:

* font-size
* font-family or font

The short version of ``'font-family'`` is ``'font'``; they are synonymous.

Possible font-families are serif, sans-seif, monospace; Times, Helvitica
Coureir, ZapfDingbats, and Symbol. Since these font families are buiit in,
they are guarenteed to work, regardless of the availability of fonts on 
an operating system.

Body
=====

The following properties can be set for the body of the document. The body
is defined as everything except headers and footers.

* font-size
* font
* line-height or line-spacing

The property ``'line-spacing'`` is the short version of ``'line-height'``;
They are synomous. 

Set ``line-spacing`` to a number to determine the spacing relative to
the font size. For example, a value of ``'2'`` sets the line spacing to
double. 

Set ``'line-spacing'`` to a measure, such as ``'24pt'`` to set an absolute
value to the line spacing.
