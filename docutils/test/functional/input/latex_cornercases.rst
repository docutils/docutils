Additional Tests for the LaTeX Writer
*************************************

These tests contain syntax elements and combinations which may cause
trouble for the LaTeX writer.

.. contents::

.. include:: data/section_titles.rst
.. include:: data/option_lists.rst

Block Quotes
============

    This block quote comes directly after the section heading and is
    followed by a paragraph.

    This is the second paragraph of the block quote and it contains
    some more text filling up some space which would otherwise be
    empty.

    -- Attribution

This is a paragraph.

    This block quote does not have an attribution.

This is another paragraph.

    Another block quote at the end of the section.


More Block Quotes
=================

    Block quote followed by a transition.

----------

    Another block quote.


Images
======

Image with 20% width:

.. image:: ../../../docs/user/rst/images/title.png
   :width: 20%

Image with 100% width:

.. image:: ../../../docs/user/rst/images/title.png
   :width: 100%

.. include:: data/tables_latex.rst
.. include:: data/nonalphanumeric.rst
.. include:: data/unicode.rst
.. include:: data/latex_encoding.rst
.. include:: data/hyperlinking.rst
.. include:: data/urls.rst
