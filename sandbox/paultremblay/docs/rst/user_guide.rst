Page Layout
=============

Paper Size
------------

The following determmine the size of the paper:

Height
+++++++

* default-simple-page-master.page-height = 11in
* paper.height = 11in
* paper-size.height = 11in

Width
++++++

* default-simple-page-master.page-width = 8.5in
* paper.width = 8.5in
* paper-size.width = 8.5in

Page Margins
---------------

For a Simple Document
+++++++++++++++++++++++

Each set of properties has the same effect.

* page.top-margin = .75in
* page.bottom-margin = .75in
* page.right-margin = .75in
* page.left-margin = .75in
 
* page.margin-top = .75in
* page.margin-bottom = .75in
* page.margin-right = .75in
* page.margin-left = .75in

* page-layout.top-margin = .75in
* page-layout.bottom-margin = .75in
* page-layout.right-margin = .75in
* page-layout.left-margin = .75in

* page-layout.margin-top = .75in
* page-layout.margin-bottom = .75in
* page-layout.margin-right = .75in
* page-layout.margin-left = .75in

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

The short version:

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
