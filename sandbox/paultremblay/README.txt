=========================
 README: Docutils2fo 0.6
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
 docutils_to_fo.py my_file.xml > my_file.fo
 fop my_file.fo my_file.pdf


Completion
==========

* Ability to create custom pages of simple layout, of first page layout,
  of odd-even layout, and first, odd-even layout.
* Sections
* Transitions
* Paragraphs
* Bullet Lists
* Enumerated Lists
* Definition Lists
* Field Lists
* Bibliographic Fields
* Option List
* Line Blocks
* Block Quotes
* Headers and Footers, with the ability to suppress them on the first
  page.
* Doctest Blocks
* Tables
* Footnotes
* Citations
* Hyperlinks
* Interpreted Text
* inline Literals
* Specific Admonitions
* Generic Admonition
* Image
* Figure
* Topic
* Sidebar
* Parsed Literal Block
* Rubric
* Epigraph
* Highlights
* Pull-Quote
* Compound Paragraph
* Container

* documenttion of XSLT stylesheets
* Develop code to process XSlT stylesheet (using lxml)

TODO
=====


Python Code
------------

- Develop code to read configuration file (in process of doing)

- Develop code to check configuration file (in process of doing)

- Develop code to output a customized XSLT stylesheet (in process)



Documentaion
------------


- Write a quick overview

- Write a detailed overview

Limitations
------------

* Cannot do transitions as well as latex
* Cannot put borders around page
* Cannot do sidebars
