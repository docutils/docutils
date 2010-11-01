.. section-numbering::

.. class:: c1

.. class:: c2

==============================
reStructuredText Test Document
==============================

.. class:: c3

-----------------------------
Examples of Syntax Constructs
-----------------------------

.. bibliographic fields (which also require a transform):

.. class:: c4

:Author: David Goodger

.. class:: c5

:Address: 123 Example Street Example, EX Canada A1B 2C3

:Contact: goodger@users.sourceforge.net

:Authors: Me; Myself; I

:organization: humankind

:date: Now, or yesterday. Or maybe even *before* yesterday.

:status: This is a "work in progress"

:revision: is managed by a version control system.

:version: 1

:copyright: This document has been placed in the public domain. You may do with
    it as you wish. You may copy, modify, redistribute, reattribute,
    sell, buy, rent, lease, destroy, or improve it, quote it at length,
    excerpt, incorporate, collate, fold, staple, or mutilate it, or do
    anything else to it that your or anyone else's heart desires.

.. class:: c6

:field name: This is a "generic bibliographic field".

:field name "2": Generic bibliographic fields may contain multiple body elements.

    .. class:: c7

    Like this.

.. class:: c8

:Dedication:

    .. class:: c9

    For Docutils users & co-developers.

.. class:: c10

:abstract: This is a test document, containing at least one example of each
    reStructuredText construct.

.. class:: c11

.. contents:: Table of Contents
   :class: c12

.. class:: c13

Structural Elements
===================

.. class:: c14

Section Title
-------------

.. class:: c15

That's it, the text just above this line.

.. class:: c16

-----

.. class:: c17

A paragraph.

Bullet Lists
------------

.. class:: c18

- A bullet list

  .. class:: c19

  + Nested bullet list.

  .. class:: c20

  + Nested item 2.

Enumerated Lists
----------------

.. class:: c21

1. Arabic numerals.

   .. class:: c22

   a) lower alpha)

Definition Lists
----------------

.. class:: c23

Term
    .. class:: c24

    Definition

Field Lists
-----------

.. class:: c25

:how arg1 arg2:

    .. class:: c26

    The field marker is a colon, the field name, and a colon.

    .. class:: c27

    The field body may contain one or more body elements, indented
    relative to the field marker.

Option Lists
------------

For listing command-line options:

.. class:: c28

-a
    command-line option "a"

.. class:: c29

-x, -y, -z
    Multiple options are an "option group".

Literal Blocks
--------------

Literal blocks are indicated with a double-colon ("::") at the end of
the preceding paragraph (over there ``-->``). They can be indented

.. class:: c30

::

    if literal_block:
        text = 'is left as-is'
        spaces_and_linebreaks = 'are preserved'
        markup_processing = None

Or they can be quoted without indentation

.. class:: c31

::

>> Great idea!
>
> Why didn't I think of that?

Line Blocks
-----------

This section tests line blocks. Line blocks are body elements which
consist of lines and other line blocks. Nested line blocks cause
indentation.

.. class:: c32

| This is a line block. It ends with a blank line.

.. class:: c33

|     New lines begin with a vertical bar ("|").

Block Quotes
------------

Block quotes consist of indented body elements:

    .. class:: c34

    My theory by A. Elk. Brackets Miss, brackets. This theory goes as
    follows and begins now. All brontosauruses are thin at one end, much
    much thicker in the middle and then thin again at the far end. That
    is my theory, it is mine, and belongs to me and I own it, and what
    it is too.

    .. class:: c35

    -- Anne Elk (Miss)

Doctest Blocks
--------------

.. class:: c36

>>> print 'Python-specific usage examples; begun with ">>>"'
Python-specific usage examples; begun with ">>>"
>>> print '(cut and pasted from interactive Python sessions)'
(cut and pasted from interactive Python sessions)

Footnotes
---------

.. class:: c37

.. [1] A footnote contains body elements, consistently indented by at least
   3 spaces.

   .. class:: c38

   This is the footnote's second paragraph.

.. class:: c39

.. [#label] Footnotes may be numbered, either manually (as in [1]_) or
   automatically using a "#"-prefixed label. This footnote has a label
   so it can be referred to from multiple places, both as a footnote
   reference ([#label]_) and as a hyperlink reference (label_).

.. class:: c40

.. [#] This footnote is numbered automatically and anonymously using a label
   of "#" only.

.. class:: c41

.. [*] Footnotes may also use symbols, specified with a "*" label. Here's a
   reference to the next footnote: [*]_.

Citations
---------

.. class:: c42

.. [CIT2002] Citations are text-labeled footnotes. They may be rendered separately
   and differently from footnotes.

Targets
-------

.. class:: c43

.. _example:

.. class:: c44

.. _Python: http://www.python.org/

Some__ reference__.

.. class:: c45

__ Targets_

.. class:: c46

__ http://www.example.com/

Directives
----------

.. contents:: :local:
   :class: c47

Images
~~~~~~

An image directive (also clickable -- a hyperlink reference):

.. image:: ../../../docs/user/rst/images/title.png
   :class: c48
   :target: directives_

A figure directive:

.. figure:: ../../../docs/user/rst/images/title.png
   :figclass: c49
   :alt: reStructuredText, the markup syntax

   A figure is an image with a caption and/or a legend:

   .. class:: c50

   +------------+-----------------------------------------------+
   | re         | Revised, revisited, based on 're' module.     |
   +------------+-----------------------------------------------+
   | Structured | Structure-enhanced text, structuredtext.      |
   +------------+-----------------------------------------------+
   | Text       | Well it is, isn't it?                         |
   +------------+-----------------------------------------------+

Admonitions
~~~~~~~~~~~

.. class:: c51

.. attention:: Directives at large.

.. class:: c52

.. caution:: Don't take any wooden nickels.

.. class:: c53

.. danger:: Mad scientist at work!

.. class:: c54

.. error:: Does not compute.

.. class:: c55

.. hint:: It's bigger than a bread box.

.. class:: c56

.. important:: 
   - Wash behind your ears.

   - Clean up your room.

   - Call your mother.

   - Back up your data.

.. class:: c57

.. note:: This is a note.

.. class:: c58

.. tip:: 15% if the service is good.

.. class:: c59

.. warning:: Strong prose may provoke extreme mental exertion. Reader discretion
   is strongly advised.

.. admonition:: And, by the way...
   :class: c60

   You can make up your own admonition too.

Topics, Sidebars, and Rubrics
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. sidebar:: Sidebar Title
   :subtitle: Optional Subtitle
   :class: c61

   This is a sidebar. It is for text outside the flow of the main text.

   .. class:: c62

   .. rubric:: This is a rubric inside a sidebar

   Sidebars often appears beside the main text with a border and
   background color.

.. topic:: Topic Title
   :class: c63

   This is a topic.

Compound Paragraph
~~~~~~~~~~~~~~~~~~

.. compound::
   :class: c64

   Compound 1, paragraph 1.

   Compound 1, paragraph 2.

   * Compound 1, list item one.

   * Compound 1, list item two.
