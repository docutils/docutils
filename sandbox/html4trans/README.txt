..                            -*- rst-mode -*-

===============================================
HTML writer for lightweight browsers
===============================================

:Author: Guenter Milde <milde@users.BerliOS.de>
:Date: $Date$

.. contents:: 

Motivation
==========

The HTML Writer module, `docutils.writers.html4css1` produces XHTML
compatible with the XHTML 1.0 specification.  Correct rendering of the HTML
produced depends on the CSS support of the browser.  Browsers without full
CSS support (lightweight browsers, older browsers, text-only browsers, help
viewers) produce inferior results. 

CSS is the state-of-the art technology for styling of html output and
provides many advantages.
However, correct rendering of HTML+CSS requires considerable resources in
form of program code, memory space and computation time.

On older machines or in embedded devices this might pose a serious problem.
Another example is a HTML help viewer of a program. It should start up fast
("immediately") and not take too much resources.

Editing and generating documentation with reStructuredText is possible with
modest requirements on computer power. Viewing the result should be as well.

Solution
========

A separate HTML-transitional writer producing non-CSS based HTML. 

The `html4trans`_ writer module subclasses the `html4css1.HTMLTranslator`
and modifies it to remove the dependency on CSS browser support.

The output conforms to the XHTML version 1.0 Transitional DTD and contains
enough formatting information to be viewed without a cascading style sheet by
a lightweight html browser (e.g. Dillo_ or the console browser elinks_).

Drawbacks
=========

Doing without CSS styling comes at a cost. 

* The clear distinction of content and layout is lost. The HTML output
  contains a fair amount of "hard-coded" visual markup. 
* CSS style sheets can still be used but cannot override the non-CSS layout
  code. It is no longer possible to e.g. modify the colour of a warning text
  with a style sheet.

* Some more advanced features of CSS layout are hard or impossible to
  achieve with pure HTML code. Less important features, not implemented in
  this version of the "html4trans" writer are:

  * Arabic, roman and alpha list markers for enumerated lists.
  
  * Grey background for inline literal and literal blocks.
  
  * Leading indentation in line-block objects.
  
  * ``:Dedication:`` and ``:abstract:`` bibliographic fields.
  
  * Topics, Sidebars, and Rubrics.
  
  * Custom Roles with class attributes


Files
========

* `<html4trans.py>`_ Simple Hypertext Markup Language document tree Writer.

* `rst2html_trans.py`_
  Front end to the Docutils Publisher, producing HTML not requiring
  a CSS stylesheet.

* `<docs>`_ Documentation

* `<tools>`_ Front-end and debugging scripts
   
* `<data>`_ sample documents


Installation
============

Place `rst2html_trans.py`_ in the binary PATH and `html4trans.py`_ in the
PYTHONPATH or in the working directory 
(from where you start ``rst2html_trans.py``).

Usage
=====

Similar to the standard ``rst2html`` front end:: 
  
  rst2html_trans.py [options] [<source> [<destination>]]
  
The full usage_ text can be obtained with the ``--help`` option.

Links
=====

* DocArticle_ 
  is a comparable project by Bill Bumgarner. It is more specific
  in its approach, however, producing non-CSS based HTML that is compliant
  with the O'Reilly DevCenter's article submission guidelines.

* `Inside A Docutils Command-Line Front-End Tool`_
  How to roll your own Docutils front end. (Also see the other 
  `API Reference Material for Client-Developers`_.)


.. References

.. _html4trans:
.. _html4trans.py: html4trans.py
.. _rst2html_trans.py: tools/rst2html_trans.py
.. _usage: docs/usage.html

.. _dillo: http://www.dillo.org
.. _elinks: http://elinks.cz
.. _DocArticle: ../bbum/DocArticle
.. _Inside A Docutils Command-Line Front-End Tool:
     http://docutils.sourceforge.net/docs/api/cmdline-tool.html
.. _API Reference Material for Client-Developers:
     http://docutils.sourceforge.net/docs/index.html#api-api-reference-material-for-client-developers





