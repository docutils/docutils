.. readme.txt: introduction to the html4strict writer   -*- rst-mode -*-

=====================================================
HTML Writer producing strict HTML 4 relying on CSS 2.
=====================================================

..
  :Author: Guenter Milde
  :Date: $Date$

Motivation
==========

The HTML Writer module, `docutils.writers.html4css1` generates XHTML
compatible with the XHTML 1.0 specification.

However, generated documents do not conform to the "strict" variant, as they
contain some deprecated structures in order to ensure correct display with
deficient browsers.

Solution
========

A HTML writer, producing XHTML that strictly conforms to the XHTML
1.0 specification.

The `html4strict`_ writer module subclasses the `html4css1.HTMLTranslator`
and modifies it to remove deprecated HTML constructs.
The output conforms to the XHTML version 1.0 Strict DTD.

A `html4css2.css`_ stylesheet provides the standard layout for
CSS2-conforming Html browsers (e.g. Firefox_ or Opera_).
Alternative style sheets are in the `<data>`_ subdir.

Advantages
----------

* Standards compliance in the strictest sense.

* Clear distinction of content and layout. The HTML output
  contains no "hard-coded" visual markup.

* CSS style sheets can style almost every aspect of the visual appearance

* Field lists are written as definition lists and styled with CSS. This

  + improves the CSS-configurability as well as

  + reduces the loading time of a document with long field lists.

Files
-----

* `html4strict.py`_
  Hypertext Markup Language document tree Writer, producing strict XHTML,

* `<tools>`_ with front-end `rst2html_strict.py`_.

* data_ Style sheets and sample documents including

  * `html4css2.css`_ stylesheet for CSS2-conforming Html browsers,
  * `docutils.conf <data/docutils.conf>`_ example config file.

* `<docs>`_ Documentation and example output, comparing layout variants.

Installation
============

Place `rst2html_strict.py`_ in the binary PATH and `html4strict.py`_ in the
PYTHONPATH or in the working directory
(from where you start ``rst2html_strict.py``).

Usage
=====

Similar to the standard ``rst2html`` front end::

  rst2html_strict.py [options] [<source> [<destination>]]

The full usage text can be obtained with the ``--help`` option.

Some of the described options are special to html4css1 and have no effect in
the html4strict version of the HTML writer.

Links
=====

* `html4trans`_ is a similar sandbox project, a HTML writer producing XHTML
  that contains enough formatting information to be viewed without a
  cascading style sheet by a lightweight html browser (e.g. Dillo__ or the
  console browser elinks__).

__ http://www.dillo.org
__ http://elinks.cz

* `Inside A Docutils Command-Line Front-End Tool`_
  How to roll your own Docutils front end. (Also see the other
  `API Reference Material for Client-Developers`_.)

.. References

.. _html4strict:
.. _html4strict.py: html4strict.py
.. _rst2html_strict.py: tools/rst2html_strict.py
.. _html4css2.css: data/html4css2.css

.. _firefox: http://www.mozilla.com
.. _opera: http://www.opera.com

.. _html4trans: ../html4trans
.. _Inside A Docutils Command-Line Front-End Tool:
     http://docutils.sourceforge.net/docs/api/cmdline-tool.html
.. _API Reference Material for Client-Developers:
     http://docutils.sourceforge.net/docs/index.html#api-api-reference-material-for-client-developers

