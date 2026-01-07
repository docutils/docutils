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


Additional text roles
=====================

Including the "html-roles.txt" standard definition file adds
roles matching semantic HTML inline markup elements.

.. include:: <html-roles.txt>

=====  ===============  ====================================
Role   Example          Notes
=====  ===============  ====================================
del    :del:`removed`   removed content
ins    :ins:`inserted`  editional additions
b      :b:`keyword`     highlight :b:`key words`
                        without marking them up as important
dfn    :dfn:`dfn`       the defining instance of a term
i      :i:`rôle`        :i:`voix alternative`
kbd    :kbd:`Ctrl X`    user input
mark   :mark:`up`       highlight a :mark:`run of text`
q      :q:`Tagline!`    content quoted from another source
s      :s:`strike`      text that is inaccurate or
                        no longer relevant
samp   :samp:`Ready!`   computer output
small  :small:`print`   side comments
u      :u:`anotation`   unarticulated annotations of, e.g,
                        :u:`comon mispellings`
var    :var:`n`         variables (or constants)
=====  ===============  ====================================

.. include:: data/tables_latex.rst
.. include:: data/nonalphanumeric.rst
.. include:: data/unicode.rst
.. include:: data/latex_encoding.rst
.. include:: data/hyperlinking.rst
.. include:: data/urls.rst
