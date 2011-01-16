=========================
 README: Docutils2fo 0.1
=========================

:Author: Paul Tremblay
:Contact: paulhtremblay@gmail.com
:Date: $Date$
:Copyright: This document has been placed in the public domain.

.. contents::

Introduction
============

This directory contains stylesheets to convert docutils.xml to XSLFO.
Once the document is converted to XSLFO, you would convert it to PDF
in this way::

 rst2xml.py  my_file.rst > my_file.xml
 xsltproc xsl_fo/docutils_to_fo.xsl my_file.xml > my_file.fo
 fop my_file.fo my_file.pdf

Or::

 rst2xml.py  my_file.rst > my_file.xml
 xsltproc xsl_fo/docutils_to_fo.xsl my_file.xml | fop -fo - -pdf my_file.pdf

I may complete a script that will allow the user to process the file in one
line, using all python::

 rst2xml.py my_file.rst | docutils2fo.py | fop -fo - -pdf my_file.pdf

However, my main focus will be on first developing the stylesheets that will
allow this conversion.

In addition, I would like to develop a configuration file that would allow a
user to easily configure the output of the PDF. Such a configuration file
would have to be developed by consensus on the mailing list, if at all.

Completion
==========

- Ability to create custom pages of simple layout, of first page layout,
  of odd-even layout, and first, odd-even layout.

- Sections

- Transitions

- Paragraphs

- Bullet Lists

- Enumerated Lists

- Definition Lists

- Field Lists

- Headers and Footers, with the ability to suppress them on the first
  page.


TODO
=====

XSLT Stylesheets
-----------------

- Bibliographic Fields

- Option List

- Indented Literal Blocks

- Quoted Literal Blocks

- Line Blocks

- Doctest Blocks

- Tables

- Footnotes

- Citations

- Hyperlinks

- Interpreted Text

- inline Literals

- Directives

Python Code
------------

- Develop code to read configuration file

- Develop code to check configuration file

- Develop code to output a customized XSLT stylesheet

- Develop code to process XSlT stylesheet (using lxml)

- Develoop code to process RST, XSLT, and FO in one pass


Documentaion
------------

- Document XSLT stylesheets

- Write a quick overview

- Write a detailed overview

