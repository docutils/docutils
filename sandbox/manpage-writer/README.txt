==============================
 manpage writer for Docutils_ 
==============================

:Contact: grubert@users.sourceforge.net
:Revision: $Revision$
:Date: $Date$
:Copyright: This document has been placed in the public domain.

This tries to explore the posibilities to generate man-pages from
ReStructured text. Man pages are the way for unix systems to provide
help to the user. Gnu does this with info-pages.

Primary goal is to produce man pages to be viewed online, full nroff support
with tables, equations and images is not on the list, is it ?

Installation
''''''''''''

Copy ``rst2man.py`` into a directory in your path and ``writers/manpage.py``
into the writers directory of your docutils installation.

Module information
''''''''''''''''''

man pages look like ::
       
   man(1)     Man Pager Utils     man(1)
       
   NAME
       man - an interface to the on-line reference manuals
       
   SYNOPSIS
       man [-c|-w|-tZT device] [-adhu7V] [-m system[,...]] [-L locale]
     
in roff formatting ::
     
     .TH man 1 "14 May 2001" "2.3.19" "Manual pager utils"
     .SH NAME
     man \- an interface to the on-line reference manuals
     .SH SYNOPSIS
     .\" The general command line
     .B man
     .RB [\| \-c \||\| \-w \||\| \-tZT
     .IR device \|]

This means we have 

* a title "man" 
* a subtitle "an interface to the on-line reference manuals"
* a section "1"
* a group "Manual pager utils"
* a date "14 May 2001"
* a version "2.3.19"

References
''''''''''

man pages from section 7, ``man`` and ``man-pages``.

Problems
''''''''

* man pages have a special structure and organization. From the manpage 
  to *man* ::

    The table below shows the section numbers of the manual followed  by  the
    types of pages they contain.

    1   Executable programs or shell commands
    2   System calls (functions provided by the kernel)
    3   Library calls (functions within program libraries)
    4   Special files (usually found in /dev)
    5   File formats and conventions eg /etc/passwd
    6   Games
    7   Miscellaneous  (including  macro  packages and conven-
        tions), e.g. man(7), groff(7)
    8   System administration commands (usually only for root)
    9   Kernel routines [Non standard]

    A manual page consists of several parts.

    They  may  be  labelled  NAME,  SYNOPSIS,  DESCRIPTION,  OPTIONS,  FILES,
    SEE ALSO, BUGS, and AUTHOR.

    The  following  conventions apply to the SYNOPSIS section and can be used
    as a guide in other sections.

    bold text          type exactly as shown.
    italic text        replace with appropriate argument.
    [-abc]             any or all arguments within [ ] are optional.
    -a|-b              options delimited by | cannot be used together.
    argument ...       argument is repeatable.
    [expression] ...   entire expression within [ ] is repeatable.

    The command or function illustration is a pattern that should  match  all
    possible  invocations.   In some cases it is advisable to illustrate sev-
    eral exclusive invocations as is shown in the SYNOPSIS  section  of  this
    manual page.

* use title and subtitle to built ::

    NAME
       man - program to view ...

* new lines in general.

  Consecutive blank lines are merged by the viewer but not on printouts.
  So one has to be cautious. This is most disturbing when printing 
  postscript.

  NOTE: 

    1. Roff requests only work when at line start.
    2. But consecutive blank lines are merged by the viewer but not on 
       printouts.

    So try the rule start new lines in ``visit_``-functions, but only if 
    necessary. E.g. ``field-names`` are already on a new line because of 
    docutils structure.

* Indentation, left margin:

  The writer includes two macros ``.INDENT`` and ``.UNINDENT`` that
  keep track of the indentation in roff-code, for line-blocks python
  keeps track of it. WHAT should be the preferred way ?

  But standard macros like ``.PP`` might reset it.

  Current indent is in register ``.i``.

Open issues
'''''''''''

* nonproportial font for verbatim
* line ends around email or web addresses in texts.
  How to distinguish something is inline or not ?
  
* Tables are possible in roff, via the external processor tbl. Although
  advised to avoid if possible.
* Images and equations are discouraged.
* Lists in admonitions are not intended.

Change log
''''''''''

* writers/manpage.py:

  - 2009-07-26: 

    * Shrink trailing blanks lines in tables.
    * Footnotes and citations.

  - 2009-07-25: 

    * replace unicode non breaking space by roff equivalent (in case
      someone includes a table-of-contents)
    * fix line-blocks.
    * fix option formatting.

  - 2009-07-23: 

    * Shrink vertical space in lists and the header
    * Table cells with more lines.

  - 2009-07-22: replace all NotImplemented by warnings.

  - 2009-07-21: cleanups 

                * skip targets.
                * topics, sidebar and rubric.
                * Fix: protection of roff requests in literal-block.
                * docinfo status and generic fields.
  - 2009-07-20: cleanups and

                * admonitions.
                * start values for alphabetic enumerations
                * Fix: remove blank line before ``.TH``
                * Roman enumerations.
                * Docinfo: contact, authors.
  - 2009-07-16: patches from martin g: add container handling, remove blank in
                option_item
  - 2009-07-13: Fix: system_message output (no heading necessary).
  - 2009-07-13: patch from ben finney, remove blank line before list_item.
  - 2009-07-13: patch from martin geisler blank line after a literal block.
  - 2009-07-13: patch from martin geisler to propper backslash escaping.
  - 2009-07-13: patch from ben finney to reduce vertical spacing.
  - 2009-06-26: patch from jarda to fix visit_strong. 
  - 2009-06-22: page title in ``.TH`` in uppercase.
  - 2009-04-15: move ``rst2man.py`` into ``tools`` subdirectory
                Use writer_name in ``publish_cmdline``.

  - 2009-04-07: implement ``address`` and ``organization`` as line block
                in section ``AUTHOR``

  - 2008-09-21: implement ``problematic`` as line block.
  - 2008-05-16: patch from htgoebel:

                * keep ``=`` in long options like ``--filename=FILENAME``
                * Support for blockqoutes.

  - 2008-05-15: smaller things in lists

                * use ``.IP`` instead of ``.TP`` for lists
                * arabic enums might start with other than "1".
                * indentation of arabic lists depends on number of entries
                  This should be with of largest number though.
                * indent alpha lists by 3.

  - 2008-05-15: patch from htgoebel for nested intendation.
  - 2008-03-05: patch from htgoebel

                * Support formats for literal
                * references are now typeset in italics (or what ever the
                  renderer uses as italic font)
                * corrected output for emphasis
                * corrected dedenting after (bullet-, enum-, definition-,
                  option-) lists
                * leading whitespace is removed from document, thus eg the
                  tool 'file' detects the correct file-type

  - 2008-01-17: make title_references italic. Experimental table support.
  - 2007-06-01: Enforce we have a TH.
  - 2006-12-11: Add field name.
  - 2006-11-24: Add transition.
  - 2006-10-31: Add raw.
  - 2006-10-31: Distinguish section_level 0 (TH), 1 (SH) other (SS).
  - 2006-10-30: Add line block.
  - 2006-10-30: Add headerless tables.
  - 2006-10-23: Fix: listbullets and numbers were prefixed with ``0``.
  - 2006-10-22: less empty lines
  - 2006-10-21: from sandbox/grubert, better option list handling,
                definition list handling.

.. _Docutils: http://docutils.sourceforge.net/
