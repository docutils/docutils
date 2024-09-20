.. include:: ../header.rst

==============================
 manpage writer for Docutils_
==============================

:Author: Engelbert Gruber
:Contact: docutils-develop@lists.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.

This writer generates man pages from reStructuredText.  Man pages (short
for "manual pages") are the way Unix systems have provided reference
material to the user on demand, at the shell prompt or in printed and
bound manuals, since 1971.  Some GNU projects offer a similar resource
with the Texinfo system.

Man pages are usually formatted with a typesetting language and program
called ``troff``.  *man* is a macro package for ``troff``.  OpenBSD,
NetBSD, and some GNU/Linux distributions use an alternative program,
``mandoc``, that does not support any typesetting features that cannot
be exercised in a terminal emulator, but supports enough *man* and
``troff`` syntax that it can meet many users' needs.  This writer
produces a dialect that is supported by both programs.

Historically, the program for formatting on terminals is called
``nroff``.  When details of the output device are not important, we can
refer to the language and its system as *roff*.


.. contents::


Module information
==================

Man pages are organized into numbered sections.  A system's *intro*\(1)
or *man*\(1) page lists them.  For example, section 1 documents
user commands, and section 3 presents programming library functions.
You will find a "printf" page in both sections; the notations
*printf*\(1) and *printf*\(3) distinguish them.

A man page can discuss several topics; for example, the *gzip*\(1) page
documents the ``gzip``, ``gunzip``, and ``zcat`` commands.  A man page's
topic list is followed by a summary description containing key words
that enable the page to located quickly with the ``apropos`` command.

A rST document's title becomes the man page's identifier, and the
subtitle its summary description.  The title and docinfo populate the
the man page's header and footer.

Each man page is organized by a standardized set of section headings;
see References_.  Use additional section headings at your discretion
only if the standard set does not serve.  Man pages support one level of
subsection headings to further organize a section heading's material.

The beginning of a formatted man page might look like this::

    man(1)                   Manual pager utils                   man(1)

    NAME
         man - an interface to the system reference manuals

    SYNOPSIS
         man [-c|-w|-tZT device] [-adhu7V] [-m system[,...]] [-L locale]

It would have a page footer with further information::

    noman 1.2.3                  2021‐02‐08                       man(1)

It could be produced by the following *roff* input.

.. code:: man

     .TH man 1 2021-02-08 "noman 1.2.3" "Manual pager utils"
     .SH NAME
     man \- an interface to the system reference manuals
     .SH SYNOPSIS
     .B man
     .RB [ \-c | \-w | \-tZT
     .IR device ]

We can write rST to produce the foregoing.

.. code:: rst

  =====
   man
  =====

  --------------------------------------------
  an interface to the system reference manuals
  --------------------------------------------

  :Version: noman 1.2.3
  :Date: 2021-02-08
  :Manual section: 1
  :Manual group: Manual pager utils

  SYNOPSIS
  ========

  ``man`` ``[-c|-w|-tZT device] [-adhu7V] [-m system[,...]] [-L locale]``

The *man-db* project's own *man* page, which may be installed as
*man*\(1) or *gman*\(1), explains the sectional organization of the
manuals (in both senses).


Configuration
=============

Options can be specified as

* command-line options, or

* configuration settings.

Run ``rst2man --help`` to get a list of available options;
see `Docutils Configuration`_ for details.

.. _Docutils Configuration: config.html

Option ``text-references``, ``macro-references`` set the way
URI-references are typeset in the generated man page.

``text-references``: is the default (for now). A URI-reference would be
    typeset::

      see Linux Man Page Howto <https://tldp.org/HOWTO/Man-Page/>

``macro-references``: This results in the usage of *man*-macros for
    URI-references::

      see
      .UR https://tldp.org/HOWTO/Man-Page/
      Linux Man Page Howto
      .UE

    The the actual typesetting is done by man/roff/mandoc.

    In groff 1.23 the macros support OSC8 output, to notify terminals that
    some text is a link, similar to html ``<a ref="abc.html">abc</a>``,
    ``abc.html`` then wouldn't be visible to the user but the terminal
    offers opening the standard browser to that URI ... assuming OSC8 is
    supported.


Limitations
===========

The "manpage" writer does not support all `Docutils Document Tree`_ elements
but a subset that is relevant for manual pages.

The output is pure text, images are not included. Instead, the text
description in their `"alt" attribute`_ is shown.

.. TODO: a warning is given for image elements without `"alt" attribute`_.

The "manpage" writer ignores element attributes specifying a measure_.

.. _measure: ../ref/doctree.html#measure
.. _"alt" attribute: ../ref/doctree.html#alt

.. groff supports the CSS2 units except "ex", "mm", and "px"
   https://www.gnu.org/software/groff/manual/groff.html#Measurements


Conventions
===========

* Newlines, line breaks, and sentences

  One should try to comply with the `troff line break rules`__ after
  ``.``, ``?``, and ``!`` punctuation signs in the rST source.  Use a
  new line or two spaces to start a new sentence.

  Avoid line breaks after ``.``, ``?``, and ``!`` signs that do not end
  a sentence.  You can escape a line break with a backslash.  In rST,
  escaped whitespace is removed, so precede the backslash by a space::

    We recommend the works of E. T. A. \
    Hoffman.

  __ https://www.gnu.org/software/groff/manual/groff.html#Sentences

  A man page viewer on a terminal might merge consecutive blank lines,
  but this is not done on typesetting devices like printers.  Check your
  output with a PostScript or PDF viewer before printing it.

* Filenames are always in italics, except in C language
  preprocessor inclusions in the SYNOPSIS section. [LMHT]_
  Use::

    .I /usr/include/stdio.h

  and::

    .B #include <stdio.h>


References
==========

Consult man pages from section 7: *man*, *man-pages*, *groff_man* and
*groff_man_style*.

.. [LMHT] `Linux Man Page Howto <https://tldp.org/HOWTO/Man-Page/>`__.

.. _Docutils: https://docutils.sourceforge.io/
.. _Docutils Document Tree: ../ref/doctree.html
