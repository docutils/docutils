..                            -*- rst-mode -*-

===============================================
Latex Writer Variants
===============================================

:Author: Guenter Milde <milde@users.berlios.de>
:Date: $Date$

This sandbox project is a place to try out and discuss variants of the
latex2e writer.

Latex writer variants could be:

* enhancements or improvements of the "standard" latex writer, or

* custom latex writers for special needs.

See the `index`_ for already existing writer variants, e.g.

* `<latex2e_adaptive_preamble>`_ reduces the size of the generated preamble
  by only including stuff really needed in the document.

* `<latex2e_external_stylesheet>`_ replaces most of the generated preamble by
  a latex stylesheet.

* `<latex2e_headings_as_multi_line_strings.py>`_ tries to make the latex2e
  writer source more readable by the use of raw literal strings
  instead of escaped backslashes (see also
  `<latex2e_headings_as_multi_line_strings.diff>`_)

* `<latex2e_listings>`_ provides an alternative implementation of the new
  ``--literal-env`` option to select the environment for
  typesetting literal blocks.
  
  This options allows e.g. to choose `lstlisting` from the ``listings.sty`` LaTeX
  package. See also `<docs/syntax-highlight-with-listings.html>`_

The `<docs>`_ dir contains generic documentation and discussion.
Writer specific documentation is placed in the respective "literate"
source.

.. _index: .

---------

See also the latex-writer related sandbox projects

.. `<an_anonymous_relative_link>`__

* `<../dkuhlman/Docs>`__,
* `<../docpy-writer>`__,
* `<../jensj/latex_math>`__,
* `<../latex_directive>`__, and
* `<../verse-writer>`__.

